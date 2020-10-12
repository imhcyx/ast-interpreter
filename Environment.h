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
    StackFrame *mParent;
public:
    StackFrame(StackFrame *parent) : mExprs(), mParent(parent) {
    }

    void bindDecl(Decl* decl, int val) {
       mVars[decl] = val;
    }
    int getDeclVal(Decl * decl) {
        for (StackFrame *cur = this; cur; cur = cur->mParent) {
            if (cur->mVars.find(decl) != cur->mVars.end()) {
                return cur->mVars[decl];
            }
        }
        llvm::errs() << "variable not found\n";
        abort();
    }
    void bindStmt(Stmt * stmt, int val) {
        mExprs[stmt] = val;
    }
    int getStmtVal(Stmt * stmt) {
        if (IntegerLiteral *lit = dyn_cast<IntegerLiteral>(stmt)) {
            return lit->getValue().getSExtValue();
        }
        assert (mExprs.find(stmt) != mExprs.end());
        return mExprs[stmt];
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
