//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool --------------===//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/EvaluatedExprVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;

#include "Environment.h"

class InterpreterVisitor : 
    public EvaluatedExprVisitor<InterpreterVisitor> {
public:
    explicit InterpreterVisitor(const ASTContext &context, TranslationUnitDecl * unit)
    : EvaluatedExprVisitor(context), mStack(), mFree(NULL), mMalloc(NULL), mInput(NULL), mOutput(NULL), mEntry(NULL) {
        for (TranslationUnitDecl::decl_iterator i =unit->decls_begin(), e = unit->decls_end(); i != e; ++ i) {
            if (FunctionDecl * fdecl = dyn_cast<FunctionDecl>(*i) ) {
                if (fdecl->getName().equals("FREE")) mFree = fdecl;
                else if (fdecl->getName().equals("MALLOC")) mMalloc = fdecl;
                else if (fdecl->getName().equals("GET")) mInput = fdecl;
                else if (fdecl->getName().equals("PRINT")) mOutput = fdecl;
                else if (fdecl->getName().equals("main")) mEntry = fdecl;
            }
        }
        mStack.push_back(StackFrame());
    }

    virtual ~InterpreterVisitor() {}

    void start() {
        // TODO: implement as function call instead
        VisitStmt(mEntry->getBody());
    }

    virtual void VisitUnaryOperator(UnaryOperator * uop) {
        VisitStmt(uop);

        Expr *sub = uop->getSubExpr();
        int val = mStack.back().getStmtVal(sub);
        int result = 0;

        switch (uop->getOpcode()) {
            case UO_Plus:   result = val;       break;
            case UO_Minus:  result = -val;      break;
        }
        mStack.back().bindStmt(uop, result);
    }

    virtual void VisitBinaryOperator(BinaryOperator * bop) {
        VisitStmt(bop);

        Expr * left = bop->getLHS();
        Expr * right = bop->getRHS();

        if (bop->isAssignmentOp()) {
            int val = mStack.back().getStmtVal(right);
            if (DeclRefExpr * declexpr = dyn_cast<DeclRefExpr>(left)) {
                Decl * decl = declexpr->getFoundDecl();
                mStack.back().bindDecl(decl, val);
            }
        }
        else {
            int lhs = mStack.back().getStmtVal(left);
            int rhs = mStack.back().getStmtVal(right);
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
            mStack.back().bindStmt(bop, result);
            //llvm::errs() << "binop " << bop->getOpcodeStr() << ": " << result << "\n";
        }
    }

    virtual void VisitDeclRefExpr(DeclRefExpr * expr) {
        VisitStmt(expr);
        mStack.back().setPC(expr);
        if (expr->getType()->isIntegerType()) {
            Decl* decl = expr->getFoundDecl();

            int val = mStack.back().getDeclVal(decl);
            mStack.back().bindStmt(expr, val);
        }
    }

    virtual void VisitCastExpr(CastExpr * expr) {
        VisitStmt(expr);
        mStack.back().setPC(expr);
        if (expr->getType()->isIntegerType()) {
            Expr * sub = expr->getSubExpr();
            int val = mStack.back().getStmtVal(sub);
            mStack.back().bindStmt(expr, val);
        }
    }

    virtual void VisitCallExpr(CallExpr * call) {
        VisitStmt(call);
        mStack.back().setPC(call);
        int val = 0;
        FunctionDecl * callee = call->getDirectCallee();
        if (callee == mInput) {
           llvm::errs() << "Please Input an Integer Value : ";
           scanf("%d", &val);

           mStack.back().bindStmt(call, val);
        } else if (callee == mOutput) {
            Expr * decl = call->getArg(0);
            val = mStack.back().getStmtVal(decl);
            llvm::errs() << val;
        } else {
            /// You could add your code here for Function call Return
        }
    }

    virtual void VisitIfStmt(IfStmt *stmt) {
        Expr *cond = stmt->getCond();

        if (Visit(cond), mStack.back().getStmtVal(cond)) {
            Visit(stmt->getThen());
        }
        else {
            Visit(stmt->getElse());
        }
    }

    virtual void VisitWhileStmt(WhileStmt *stmt) {
        Expr *cond = stmt->getCond();

        while (Visit(cond), mStack.back().getStmtVal(cond)) {
            Visit(stmt->getBody());
        }
    }

    virtual void VisitForStmt(ForStmt *stmt) {
        Stmt *init = stmt->getInit();
        Expr *cond = stmt->getCond();
        Expr *inc = stmt->getInc();

        for (Visit(init); Visit(cond), mStack.back().getStmtVal(cond); Visit(inc)) {
            Visit(stmt->getBody());
        }
    }

    virtual void VisitDeclStmt(DeclStmt * declstmt) {
        for (DeclStmt::decl_iterator it = declstmt->decl_begin(), ie = declstmt->decl_end();
                it != ie; ++ it) {
            Decl * decl = *it;
            if (VarDecl * vardecl = dyn_cast<VarDecl>(decl)) {
                mStack.back().bindDecl(vardecl, 0);
            }
        }
    }

private:
    std::vector<StackFrame> mStack;

    FunctionDecl * mFree;				/// Declartions to the built-in functions
    FunctionDecl * mMalloc;
    FunctionDecl * mInput;
    FunctionDecl * mOutput;

    FunctionDecl * mEntry;
};

class InterpreterConsumer : public ASTConsumer {
public:
    explicit InterpreterConsumer(const ASTContext& context) {}
    virtual ~InterpreterConsumer() {}

    virtual void HandleTranslationUnit(clang::ASTContext &Context) {
        TranslationUnitDecl * decl = Context.getTranslationUnitDecl();
        InterpreterVisitor *visitor = new InterpreterVisitor(Context, decl);

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
