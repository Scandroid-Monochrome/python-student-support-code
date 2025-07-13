import ast
from ast import *
from utils import *
from x86_ast import *
import os
from typing import List, Tuple, Set, Dict

Binding = Tuple[Name, expr]
Temporaries = List[Binding]


class Compiler:

    ############################################################################
    # Partial Evaluation
    ############################################################################

    def pe_neg(self, r):
        match r:
            case Constant(n):
                return Constant(neg64(n))
            case _:
                return UnaryOp(USub(), r)

    def pe_add(self, r1, r2):
        match (r1, r2):
            case (Constant(n1), Constant(n2)):
                return Constant(add64(n1, n2))
            case _:
                return BinOp(r1, Add(), r2)

    def pe_sub(self, r1, r2):
        match (r1, r2):
            case (Constant(n1), Constant(n2)):
                return Constant(sub64(n1, n2))
            case _:
                return BinOp(r1, Sub(), r2)
            
    def pe_exp(self, e, env):
        match e:
            case BinOp(left, Add(), right):
                leftExp = self.pe_exp(left, env)
                rightExp = self.pe_exp(right, env)
                return self.pe_add(leftExp, rightExp)
            case BinOp(left, Sub(), right):
                return self.pe_sub(self.pe_exp(left, env), self.pe_exp(right, env))
            case UnaryOp(USub(), v):
                return self.pe_neg(self.pe_exp(v, env))
            case Constant(value):
                return e
            case Call(Name('input_int'), []):
                return e
            
    def pe_stmt(self, s, env):
        match s:
            case Expr(Call(Name('print'), [arg])):
                return Expr(Call(Name('print'), [self.pe_exp(arg, env)]))
            case Expr(value):
                return Expr(self.pe_exp(value, env))
    
    def partial_eval(self, p: Module, env) -> Module:
        match p:
            case Module(body):
                new_body = [self.pe_stmt(s, env) for s in body]
                return Module(new_body)

    # def partial_eval(self, p: Module) -> Module:
    #     return p

    ############################################################################
    # Remove Complex Operands
    ############################################################################
    """
    def rco_exp(self, e: expr, need_atomic : bool) -> Tuple[expr, Temporaries]:
        # YOUR CODE HERE
        pass        

    def rco_stmt(self, s: stmt) -> List[stmt]:
        # YOUR CODE HERE
        pass        

    def remove_complex_operands(self, p: Module) -> Module:
        # YOUR CODE HERE
        return p                
    """
    ############################################################################
    # Select Instructions
    ############################################################################
    """
    # The expression e passed to select_arg should furthermore be an atom.
    # (But there is no type for atoms, so the type of e is given as expr.)
    def select_arg(self, e: expr) -> arg:
        # YOUR CODE HERE
        pass        

    def select_stmt(self, s: stmt) -> List[instr]:
        # YOUR CODE HERE
        pass        

    def select_instructions(self, p: Module) -> X86Program:
        # YOUR CODE HERE
        pass        
    """
    ############################################################################
    # Assign Homes
    ############################################################################
    """
    def assign_homes_arg(self, a: arg, home: Dict[Variable, arg]) -> arg:
        # YOUR CODE HERE
        pass        

    def assign_homes_instr(self, i: instr,
                           home: Dict[Variable, arg]) -> instr:
        # YOUR CODE HERE
        pass        

    def assign_homes(self, p: X86Program) -> X86Program:
        # YOUR CODE HERE
        pass        
    """
    ############################################################################
    # Patch Instructions
    ############################################################################
    """
    def patch_instr(self, i: instr) -> List[instr]:
        # YOUR CODE HERE
        pass        

    def patch_instructions(self, p: X86Program) -> X86Program:
        # YOUR CODE HERE
        pass        
    """
    ############################################################################
    # Prelude & Conclusion
    ############################################################################
    """
    def prelude_and_conclusion(self, p: X86Program) -> X86Program:
        # YOUR CODE HERE
        pass        
    """
