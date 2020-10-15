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

class AddressSpace {
    // This class stores heap and arrays in stack
    std::vector<int> mAllocs;
public:
    AddressSpace() : mAllocs() {}

    int &operator [](int ptr) {
        return mAllocs[ptr];
    }

    int alloc(int size) {
        int ptr = mAllocs.size() * sizeof(int);
        while (size > 0) {
            mAllocs.push_back(0);
            size -= sizeof(int);
        }
        return ptr;
    }

    void free(int ptr) {
        // not implemented
    }
};