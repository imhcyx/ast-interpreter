//==--- tools/clang-check/ClangInterpreter.cpp - Clang Interpreter tool --------------===//
//===----------------------------------------------------------------------===//
#include <stdio.h>

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/Decl.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Tooling.h"

using namespace clang;

class StackFrame {
    /// StackFrame maps Variable Declaration to Value
    /// Which are either integer or addresses (also represented using an Integer value)
    std::map<Decl*, int> mVars;
    std::map<Stmt*, int> mExprs;
    /// The current stmt
    Stmt * mPC;
public:
    StackFrame() : mVars(), mExprs(), mPC() {
    }

    void bindDecl(Decl* decl, int val) {
       mVars[decl] = val;
    }
    int getDeclVal(Decl * decl) {
       assert (mVars.find(decl) != mVars.end());
       return mVars.find(decl)->second;
    }
    void bindStmt(Stmt * stmt, int val) {
        mExprs[stmt] = val;
    }
    int getStmtVal(Stmt * stmt) {
        if (IntegerLiteral *lit = dyn_cast<IntegerLiteral>(stmt)) {
            //llvm::errs() << "literal: " << lit->getValue().getSExtValue() << "\n";
            return lit->getValue().getSExtValue();
        }
        assert (mExprs.find(stmt) != mExprs.end());
        return mExprs[stmt];
    }
    void setPC(Stmt * stmt) {
        mPC = stmt;
    }
    Stmt * getPC() {
        return mPC;
    }
};

/// Heap maps address to a value
/*
class Heap {
public:
    int Malloc(int size) ;
    void Free (int addr) ;
    void Update(int addr, int val) ;
    int get(int addr);
};
*/

class Environment {
    std::vector<StackFrame> mStack;

    FunctionDecl * mFree;				/// Declartions to the built-in functions
    FunctionDecl * mMalloc;
    FunctionDecl * mInput;
    FunctionDecl * mOutput;

    FunctionDecl * mEntry;
public:
    /// Get the declartions to the built-in functions
    Environment() : mStack(), mFree(NULL), mMalloc(NULL), mInput(NULL), mOutput(NULL), mEntry(NULL) {
    }


    /// Initialize the Environment
    void init(TranslationUnitDecl * unit) {
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

    FunctionDecl * getEntry() {
        return mEntry;
    }

    void unop(UnaryOperator *uop) {
        Expr *sub = uop->getSubExpr();
        int val = mStack.back().getStmtVal(sub);
        int result = 0;

        switch (uop->getOpcode()) {
            case UO_Plus:   result = val;       break;
            case UO_Minus:  result = -val;      break;
            default:
                llvm::errs() << "Unsupported unary operator " << uop->getOpcodeStr(uop->getOpcode()) << "\n";
                break;
        }
        mStack.back().bindStmt(uop, result);
    }

    void binop(BinaryOperator *bop) {
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
                case BO_LT:     result = lhs < rhs;     break;
                case BO_EQ:     result = lhs == rhs;    break;
                default:
                    llvm::errs() << "Unsupported binary operator " << bop->getOpcodeStr() << "\n";
                    break;
            }
            mStack.back().bindStmt(bop, result);
        }
    }

    void decl(DeclStmt * declstmt) {
        for (DeclStmt::decl_iterator it = declstmt->decl_begin(), ie = declstmt->decl_end();
                it != ie; ++ it) {
            Decl * decl = *it;
            if (VarDecl * vardecl = dyn_cast<VarDecl>(decl)) {
                mStack.back().bindDecl(vardecl, 0);
            }
        }
    }
    void declref(DeclRefExpr * declref) {
        mStack.back().setPC(declref);
        if (declref->getType()->isIntegerType()) {
            Decl* decl = declref->getFoundDecl();

            int val = mStack.back().getDeclVal(decl);
            mStack.back().bindStmt(declref, val);
        }
    }

    void cast(CastExpr * castexpr) {
        mStack.back().setPC(castexpr);
        if (castexpr->getType()->isIntegerType()) {
            Expr * expr = castexpr->getSubExpr();
            int val = mStack.back().getStmtVal(expr);
            mStack.back().bindStmt(castexpr, val );
        }
    }

    /// !TODO Support Function Call
    void call(CallExpr * callexpr) {
        mStack.back().setPC(callexpr);
        int val = 0;
        FunctionDecl * callee = callexpr->getDirectCallee();
        if (callee == mInput) {
           llvm::errs() << "Please Input an Integer Value : ";
           scanf("%d", &val);

           mStack.back().bindStmt(callexpr, val);
        } else if (callee == mOutput) {
            Expr * decl = callexpr->getArg(0);
            val = mStack.back().getStmtVal(decl);
            llvm::errs() << val;
        } else {
            /// You could add your code here for Function call Return
        }
    }
};


