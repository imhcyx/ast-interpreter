//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool --------------===//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;

#include "Environment.h"

class InterpreterReturnException {
public:
    InterpreterReturnException(int retval) : mRetVal(retval) {}
    int getRetVal() const { return mRetVal; }
private:
    int mRetVal;
};

class InterpreterVisitor : 
    public EvaluatedExprVisitor<InterpreterVisitor> {
public:
    explicit InterpreterVisitor(const ASTContext &context)
    : EvaluatedExprVisitor(context), mStack(), mAS(), mFree(NULL), mMalloc(NULL), mInput(NULL), mOutput(NULL), mEntry(NULL) {
    }

    virtual ~InterpreterVisitor() {
        for (StackFrame *p: mStack) {
            delete p;
        }
    }

    void init(TranslationUnitDecl * unit) {
        mStack.push_back(new StackFrame(NULL));
        for (TranslationUnitDecl::decl_iterator i =unit->decls_begin(), e = unit->decls_end(); i != e; ++ i) {
            if (FunctionDecl * fdecl = dyn_cast<FunctionDecl>(*i) ) {
                if (fdecl->getName().equals("FREE")) mFree = fdecl;
                else if (fdecl->getName().equals("MALLOC")) mMalloc = fdecl;
                else if (fdecl->getName().equals("GET")) mInput = fdecl;
                else if (fdecl->getName().equals("PRINT")) mOutput = fdecl;
                else if (fdecl->getName().equals("main")) mEntry = fdecl;
                mStack.back()->bindDecl(fdecl, -1); // fix errors when visiting a function DeclRefExpr
            }
            else if (VarDecl * vdecl = dyn_cast<VarDecl>(*i)) {
                HandleVarDecl(vdecl);
            }
        }
    }

    void start() {
        CallUserFunction(mEntry, 0, NULL);
    }

    virtual void VisitUnaryOperator(UnaryOperator * uop) {
        VisitStmt(uop);

        Expr *sub = uop->getSubExpr();
        int val = mStack.back()->getStmtVal(sub);
        int result = 0;

        switch (uop->getOpcode()) {
            case UO_Plus:   result = val;       break;
            case UO_Minus:  result = -val;      break;
            case UO_Deref:  result = mAS[val];  break;
            // TODO: dereference
        }
        mStack.back()->bindStmt(uop, result);
    }

    virtual void VisitBinaryOperator(BinaryOperator * bop) {
        VisitStmt(bop);

        Expr * left = bop->getLHS();
        Expr * right = bop->getRHS();

        if (bop->isAssignmentOp()) {
            int val = mStack.back()->getStmtVal(right);
            if (DeclRefExpr * declexpr = dyn_cast<DeclRefExpr>(left)) {
                Decl * decl = declexpr->getFoundDecl();
                mStack.back()->bindDecl(decl, val);
            }
            else if (ArraySubscriptExpr *expr = dyn_cast<ArraySubscriptExpr>(left)) {
                VisitStmt(expr);

                Expr *lhs = expr->getLHS();
                Expr *rhs = expr->getRHS();
                int lval = mStack.back()->getStmtVal(lhs);
                int rval = mStack.back()->getStmtVal(rhs);

                mAS[lval + rval] = val;
            }
            else if (UnaryOperator *expr = dyn_cast<UnaryOperator>(left)) {
                if (expr->getOpcode() == UO_Deref) {
                    VisitStmt(expr);

                    Expr *sub = expr->getSubExpr();
                    int subval = mStack.back()->getStmtVal(sub);

                    mAS[subval] = val;
                }
            }
        }
        else {
            int lhs = mStack.back()->getStmtVal(left);
            int rhs = mStack.back()->getStmtVal(right);
            int result = 0;
            switch (bop->getOpcode()) {
                case BO_Add:    result = lhs + rhs;     break;
                case BO_Sub:    result = lhs - rhs;     break;
                case BO_Mul:    result = lhs * rhs;     break;
                case BO_Div:    result = lhs / rhs;     break;
                case BO_GT:     result = lhs > rhs;     break;
                case BO_GE:     result = lhs >= rhs;    break;
                case BO_LT:     result = lhs < rhs;     break;
                case BO_LE:     result = lhs <= rhs;    break;
                case BO_EQ:     result = lhs == rhs;    break;
            }
            mStack.back()->bindStmt(bop, result);
            //llvm::errs() << "binop " << bop->getOpcodeStr() << ": " << result << "\n";
        }
    }

    virtual void VisitUnaryExprOrTypeTraitExpr(UnaryExprOrTypeTraitExpr *expr) {
        int val = 0;
        if (expr->getKind() == UETT_SizeOf) {
            if (expr->isArgumentType()) {
                const Type *t = expr->getArgumentType().getTypePtr();
                if (t->isIntegerType()) {
                    // simplified implementation: regard all integer types as int
                    val = 4;
                }
                else if (t->isPointerType()) {
                    val = 4;
                }
            }
        }
        mStack.back()->bindStmt(expr, val);
    }

    virtual void VisitParenExpr(ParenExpr *expr) {
        VisitStmt(expr);

        int val = mStack.back()->getStmtVal(expr->getSubExpr());
        mStack.back()->bindStmt(expr, val);
    }

    virtual void VisitArraySubscriptExpr(ArraySubscriptExpr *expr) {
        VisitStmt(expr);

        Expr *lhs = expr->getLHS();
        Expr *rhs = expr->getRHS();
        int lval = mStack.back()->getStmtVal(lhs);
        int rval = mStack.back()->getStmtVal(rhs);

        //llvm::errs() << "array ref: " << lval << "+" << rval << "\n";
        mStack.back()->bindStmt(expr, mAS[lval + rval]);
    }

    virtual void VisitDeclRefExpr(DeclRefExpr * expr) {
        VisitStmt(expr);
        Decl* decl = expr->getFoundDecl();

        int val = mStack.back()->getDeclVal(decl);
        mStack.back()->bindStmt(expr, val);
    }

    virtual void VisitCastExpr(CastExpr * expr) {
        VisitStmt(expr);
        Expr * sub = expr->getSubExpr();
        int val = mStack.back()->getStmtVal(sub);
        mStack.back()->bindStmt(expr, val);
    }

    virtual void VisitCallExpr(CallExpr * call) {
        VisitStmt(call);
        int val = 0;
        FunctionDecl * callee = call->getDirectCallee();
        if (callee == mInput) {
            llvm::errs() << "Please Input an Integer Value : ";
            scanf("%d", &val);

            mStack.back()->bindStmt(call, val);
        } else if (callee == mOutput) {
            Expr * decl = call->getArg(0);
            val = mStack.back()->getStmtVal(decl);
            llvm::errs() << val;
        } else if (callee == mMalloc) {
            Expr * decl = call->getArg(0);
            val = mStack.back()->getStmtVal(decl);
            mStack.back()->bindStmt(call, mAS.alloc(val));
        } else if (callee == mFree) {
            // not implemented
        } else {
            int argcount = call->getNumArgs();
            int *args = new int(argcount);
            for (int i = 0; i < argcount; i++) {
                args[i] = mStack.back()->getStmtVal(call->getArg(i));
            }
            val = CallUserFunction(callee, argcount, args);
            delete args;
            mStack.back()->bindStmt(call, val);
        }
    }

    virtual void VisitIfStmt(IfStmt *stmt) {
        Expr *cond = stmt->getCond();

        if (Visit(cond), mStack.back()->getStmtVal(cond)) {
            Visit(stmt->getThen());
        }
        else {
            Stmt *elsestmt = stmt->getElse();
            if (elsestmt)
                Visit(elsestmt);
        }
    }

    virtual void VisitWhileStmt(WhileStmt *stmt) {
        Expr *cond = stmt->getCond();

        while (Visit(cond), mStack.back()->getStmtVal(cond)) {
            Visit(stmt->getBody());
        }
    }

    virtual void VisitForStmt(ForStmt *stmt) {
        Stmt *init = stmt->getInit();
        Expr *cond = stmt->getCond();
        Expr *inc = stmt->getInc();

        if (init) Visit(init);
        for (;;) {
            if (cond) {
                Visit(cond);
                if (!mStack.back()->getStmtVal(cond)) break;
            }
            Visit(stmt->getBody());
            if (inc) Visit(inc);
        }
    }

    virtual void VisitReturnStmt(ReturnStmt *stmt) {
        Expr *expr = stmt->getRetValue();
        Visit(expr);

        int val = mStack.back()->getStmtVal(expr);
        throw InterpreterReturnException(val);
    }

    virtual void VisitDeclStmt(DeclStmt * declstmt) {
        for (DeclStmt::decl_iterator it = declstmt->decl_begin(), ie = declstmt->decl_end();
                it != ie; ++ it) {
            Decl * decl = *it;
            if (VarDecl * vardecl = dyn_cast<VarDecl>(decl)) {
                HandleVarDecl(vardecl);
            }
        }
    }

private:
    std::vector<StackFrame*> mStack;
    AddressSpace mAS;

    FunctionDecl * mFree;				/// Declartions to the built-in functions
    FunctionDecl * mMalloc;
    FunctionDecl * mInput;
    FunctionDecl * mOutput;

    FunctionDecl * mEntry;

    int CallUserFunction(FunctionDecl *callee, int argcount, int *args) {
        int retval = 0;

        mStack.push_back(new StackFrame(mStack.back()));
        for (int i = 0; i < argcount; i++) {
            mStack.back()->bindDecl(callee->getParamDecl(i), args[i]);
        }

        try {
            VisitStmt(callee->getBody());
        }
        catch (InterpreterReturnException e) {
            retval = e.getRetVal();
        }

        delete mStack.back();
        mStack.pop_back();
        return retval;
    }

    void HandleVarDecl(VarDecl *decl) {
        int val = 0;
        const Type *ty = decl->getType().getTypePtr();
        if (const ConstantArrayType *aty = dyn_cast<ConstantArrayType>(ty)) {
            int len = aty->getSize().getSExtValue();
            val = mAS.alloc(len * sizeof(int));
        }
        else if (Expr *init = decl->getInit()) {
            Visit(init);
            val = mStack.back()->getStmtVal(init);
        }
        //llvm::errs() << "decl " << decl->getDeclName().getAsString() << "(" << decl << "): " << val << "\n";
        mStack.back()->bindDecl(decl, val);
    }
};

class InterpreterConsumer : public ASTConsumer {
public:
    explicit InterpreterConsumer(const ASTContext& context) {}
    virtual ~InterpreterConsumer() {}

    virtual void HandleTranslationUnit(clang::ASTContext &Context) {
        TranslationUnitDecl * decl = Context.getTranslationUnitDecl();
        InterpreterVisitor *visitor = new InterpreterVisitor(Context);

        visitor->init(decl);
        visitor->start();

        delete visitor;
    }
};

class InterpreterClassAction : public ASTFrontendAction {
public: 
  virtual std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
     clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
     return std::unique_ptr<clang::ASTConsumer>(
          new InterpreterConsumer(Compiler.getASTContext()));
  }
};

int main (int argc, char ** argv) {
    if (argc > 1) {
         clang::tooling::runToolOnCode(std::unique_ptr<clang::FrontendAction>(new InterpreterClassAction), argv[1]);
    }
}
