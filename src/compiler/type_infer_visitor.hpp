/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

#pragma once
#include "basic/ast.hpp"
#include "basic/helper_functions.hpp"
#include "basic/type.hpp"
#include "compiler/ffi.hpp"
// type inference ... assumed to be visited after finished alpha-conversion(each
// variable has unique name regardless its scope)

namespace mimium {
struct TypevarReplaceVisitor {
  std::map<int, types::Value> tvmap;
  types::Value getTypeVarFromMap(types::TypeVar& i) {
    types::Value res;
    if (auto tvi = std::get_if<Rec_Wrap<types::TypeVar>>(&tvmap[i.index])) {
      if (tvi->getraw().index == i.index) {
        Logger::debug_log("typevar loop detected, deducted to Float",
                          Logger::WARNING);
        res = types::Float();
      } else {
        res = std::visit(*this, tvmap[i.index]);
      }
    } else {
      res = std::visit(*this, tvmap[i.index]);
    }
    return res;
  }
  // default behaviour for primitive
  types::Value operator()(types::Float& i) { return i; }
  types::Value operator()(types::String& i) { return i; }
  types::Value operator()(types::None& i) { return i; }
  types::Value operator()(types::Void& i) { return i; }

  types::Value operator()(types::Ref& i) {
    return types::Ref(std::visit(*this, i.val));
  }
  types::Value operator()(types::Pointer& i) {
    return types::Pointer(std::visit(*this, i.val));
  }
  types::Value operator()(types::TypeVar& i) {
    types::Value res;
    if (tvmap.count(i.index) > 0) {
      res = getTypeVarFromMap(i);
    } else {
      // case of fail to infer
      Logger::debug_log("failed to infer type", Logger::WARNING);
      res = types::Float();
    }
    return res;
  }
  types::Value operator()(types::Function& i) {
    std::vector<types::Value> newarg;
    for (auto& a : i.arg_types) {
      newarg.emplace_back(std::visit(*this, a));
    }
    auto newret = std::visit(*this, i.ret_type);
    return types::Function(std::move(newret), std::move(newarg));
  }
  types::Value operator()(types::Closure& i) {
    auto newcap = std::visit(*this, i.captures);
    return types::Closure((*this)(i.fun), std::move(newcap));
  };
  types::Value operator()(types::Array& i) {
    auto newelem = std::visit(*this, i.elem_type);
    return types::Array(std::move(newelem), i.size);
  }
  types::Value operator()(types::Struct& i) {
    std::vector<types::Struct::Keytype> newarg;
    for (auto& a : i.arg_types) {
      types::Struct::Keytype v = {a.field, std::visit(*this, a.val)};
      newarg.emplace_back(std::move(v));
    }
    return types::Struct(std::move(newarg));
  }
  types::Value operator()(types::Tuple& i) {
    std::vector<types::Value> newarg;
    for (auto& a : i.arg_types) {
      newarg.emplace_back(std::visit(*this, a));
    }
    return types::Tuple(std::move(newarg));
  }
  // types::Value operator()(types::Time& i) {
  //   return types::Time(std::visit(*this, i.val));
  // }
  types::Value operator()(types::Alias& i) {
    return types::Alias(i.name, std::visit(*this, i.target));
  };
  void dump();
};

struct TypeUnifyVisitor {
  explicit TypeUnifyVisitor(TypeEnv& typeenv, TypevarReplaceVisitor& tvreplacer)
      : typeenv(typeenv), tvreplacer(tvreplacer){};
  TypeEnv& typeenv;
  TypevarReplaceVisitor& tvreplacer;
  types::Value operator()(const types::None& i, const types::None& i2){
    return types::None();
  }
  types::Value operator()(const types::Void& i, const types::Void& i2){
    return types::Void();
  }
  types::Value operator()(const types::Float& i, const types::Float& i2){
    return types::Float();
  }
  types::Value operator()(const types::String& i, const types::String& i2){
    return types::String();
  }
  types::Value operator()(const types::Ref& i, const types::Ref& i2) {
    return types::Ref(std::visit(*this, i.val, i2.val));
  }
  types::Value operator()(const types::Pointer& i, const types::Pointer& i2) {
    return types::Pointer(std::visit(*this, i.val, i2.val));
  }
  types::Value operator()(const types::Function& i, const types::Function& i2) {
    std::vector<types::Value> newargs;
    auto it2 = i2.arg_types.begin();
    for (auto& a1 : i.arg_types) {
      newargs.emplace_back(std::visit(*this, a1, *it2++));
    }
    return types::Function(std::visit(*this, i.ret_type, i2.ret_type),
                           std::move(newargs));
  }
  types::Value operator()(const types::Closure& i, const types::Closure& i2) {
    return types::Closure();  // no more useful
  }
  types::Value operator()(const types::Array& i, const types::Array& i2) {
    return types::Array(std::visit(*this,i.elem_type, i2.elem_type), i.size);
  }
  types::Value operator()(const types::Struct& i, const types::Struct& i2) {
    std::vector<types::Struct::Keytype> newarg;
    auto it2 = i2.arg_types.begin();
    for (auto& a1 : i.arg_types) {
      types::Struct::Keytype nk = {a1.field,
                                   std::visit(*this, a1.val, (*it2).val)};
      newarg.emplace_back(std::move(nk));
    }
    return types::Struct(std::move(newarg));
  }
  types::Value operator()(const types::Tuple& i, const types::Tuple& i2) {
    std::vector<types::Value> newarg;
    auto it2 = i2.arg_types.begin();
    for (auto& a1 : i.arg_types) {
      newarg.emplace_back(std::visit(*this, a1, *it2));
    }
    return types::Tuple(std::move(newarg));
  }
  // llvm::Type* operator()(types::Time& i);
  types::Value operator()(const types::Alias& i, const types::Alias& i2) {
    return types::Alias(i.name, std::visit(*this, i.target, i2.target));
  }

  template <typename T>
  types::Value operator()(const types::TypeVar& i, const T& i2) {
    tvreplacer.tvmap.emplace(i.index,i2);
    return i;
  }
  types::Value operator()(const types::TypeVar& i,const types::TypeVar& i2){
    tvreplacer.tvmap.emplace(i.index,i2);
    return i;
  }
  template <typename T>
  types::Value operator()(const T& i, const types::TypeVar& i2) {
    return (*this)(i2, i);
  }
  template <typename T1, typename T2>
  types::Value operator()(const Rec_Wrap<T1>& i, const T2& i2){
    return (*this)(i.t.back(),i2);
  }
  template <typename T1, typename T2>
  types::Value operator()(const T1& i, const Rec_Wrap<T2>& i2){
    return (*this)(i,i2.t.back());
  }
  template <typename T1, typename T2>
  types::Value operator()(const Rec_Wrap<T1>& i, const Rec_Wrap<T2>& i2){
    return (*this)(i.t.back(),i2.t.back());
  }
  template <typename T1, typename T2>
  types::Value operator()(const T1& i, const T2& i2) {
    throw std::runtime_error("type mismatch detected");
    return types::None();
  }
};

class TypeInferVisitor : public ASTVisitor {
  friend TypevarReplaceVisitor;

 public:
  TypeInferVisitor();
  ~TypeInferVisitor() override = default;
  void init();
  void visit(OpAST& ast) override;
  void visit(ListAST& ast) override;
  void visit(NumberAST& ast) override;
  void visit(LvarAST& ast) override;
  void visit(RvarAST& ast) override;
  void visit(SelfAST& ast) override;
  void visit(AssignAST& ast) override;
  void visit(ArgumentsAST& ast) override;
  void visit(FcallArgsAST& ast) override;
  void visit(ArrayAST& ast) override;
  void visit(ArrayAccessAST& ast) override;
  void visit(FcallAST& ast) override;
  void visit(LambdaAST& ast) override;
  void visit(IfAST& ast) override;
  void visit(ReturnAST& ast) override;
  void visit(ForAST& ast) override;
  void visit(DeclarationAST& ast) override;
  // void visit(TimeAST& ast) override;
  void visit(StructAST& ast) override;
  void visit(StructAccessAST& ast) override;

  bool typeCheck(types::Value& lt, types::Value& rt);
  bool unify(types::Value& lt, types::Value& rt);

  bool unify(std::string lname, std::string rname);
  bool unify(std::string lname, types::Value& rt);
  // bool unifyArg(types::Value& target, types::Value& realarg);

  TypeEnv& getEnv() { return typeenv; };
  types::Value getLastType() { return res_stack.top(); }
  types::Value stackPop() {
    auto res = res_stack.top();
    res_stack.pop();
    return res;
  }
  std::string tmpfname;

  TypeEnv& infer(AST_Ptr toplevel);
  void replaceTypeVars();

 private:
  std::stack<types::Value> res_stack;
  // static bool checkArg(types::Value& fnarg, types::Value& givenarg);
  void unifyTypeVar(types::TypeVar& tv, types::Value& v);
  // hold value for infer type of "self"
  std::optional<types::Value> current_return_type;
  TypeEnv typeenv;
  TypevarReplaceVisitor tvreplacevisitor;
  TypeUnifyVisitor unifyvisitor;
  // std::unordered_map<int, types::Value> typevar_to_actual_map;
  bool has_return;
};

}  // namespace mimium