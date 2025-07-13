# Language P_var
#
# Concrete Syntax
#
# exp ::= var | int | `input_int` `(` `)` | `-` exp | exp `+` exp | exp `-` exp
# stmt ::= var `=` exp | `print` `(` exp `)` | exp
# program ::= stmt+
#
#
# Abstract Syntax
#
# exp ::= Name(var) | Constant(int) | Call(Name('input_int'), [])
#       | UnaryOp(USub(), exp) | BinOp(exp, Add(), exp)
# stmt ::= Assign([var],exp) | Expr(Call(Name('print'), [exp])) | Expr(exp)
# program ::= Module([stmt])
import ast
from ast import *
from utils import *
from x86_ast import *
import os
from typing import List, Set, Dict
import interp_Lvar
import type_check_Lvar
# import var
from eval_x86 import interp_x86
from typing import Tuple



Binding = tuple[Name, expr]
Temporaries = List[Binding]

import compiler
class Var(compiler.Compiler):
    ############################################################################
    # Partial Evaluation
    ############################################################################

    # TODO: (Exercise 2.7) Update to handle Var syntax

    def partial_eval(self, p: Module) -> Module:
        match p:
            case Module(body):
                env: Dict[str, expr] = {} # Variable storage
                new_body = []
                for s in body:
                    evaluated_stmts = self.pe_stmt(s, env)
                    if evaluated_stmts:
                        new_body.append(evaluated_stmts)
                return Module(new_body)
  
    def pe_stmt(self, s, env):
        match s:
            case Assign([Name(x)], e): # x = e
                # Evaluate the variable name
                evaluated_e = self.pe_exp(e, env)
                # Put variable name into dictionary
                env[x] = evaluated_e # returns an assignment of x such that e is self.partially evaluated
                # Return the assign statement with the simplified value
                return Assign([Name(x)], evaluated_e)
            case _:
                return super().pe_stmt(s, env) # call L_int's partial eval
    
    def pe_exp(self, e, env):
        match e:
            case Name(x):
                # If the variable exists:
                if x in env:
                    return env[x] # return the value
                # If the variable doesn't:
                else:
                    return Name(x) # return the name expression as is
            case _:
                return super().pe_exp(e, env) # returning e as super.partially eval'd

    # TODO: Uncomment and complete the below:
    ############################################################################
    # Remove Complex Operands
    ############################################################################
    
    def rco_exp(self, e: expr, need_atomic : bool) -> Tuple[expr, Temporaries]: # Temp = [(Name, exp)]
        match e:
            case Name(x):
                return Name(x), []
            case Constant(n):
                return Constant(n), []
            case BinOp(left, op, right):
                temps = []
                leftValues = self.rco_exp(left, True) #s_o must be an x or an n
                rightValues = self.rco_exp(right, True) #s_o must be an x or an n
                simplified_left = leftValues[0]
                simplified_right = rightValues[0]
                temps += leftValues[1]
                temps += rightValues[1]
                if need_atomic:
                    tmp = Name(generate_name('tmp'))
                    return tmp, temps + [(tmp, BinOp(simplified_left, op, simplified_right))]
                else:
                    return BinOp(simplified_left, op, simplified_right), temps
            case UnaryOp(op, operand):
                (simplified_operand, temps) = self.rco_exp(operand, True) #s_o must be an x or an n
                if need_atomic:
                    tmp = Name(generate_name('tmp'))
                    return tmp, temps + [(tmp, UnaryOp(op, simplified_operand))]
                else:
                    return UnaryOp(op, simplified_operand), temps
            case Call(f, args):
                simplified_args = []
                temps = []
                for arg in args:
                    # argsAndTemps: Tuple[expr, Temporaries]
                    argsAndTemps = (self.rco_exp(arg, True)) #s_o must be an x or an n
                    simplified_args.append(argsAndTemps[0])
                    temps.extend(argsAndTemps[1])

                if need_atomic:
                    tmp = Name(generate_name('tmp'))
                    return tmp, temps + [(tmp, Call(f, simplified_args))]
                else:
                    return Call(f, simplified_args), temps
            case _:
                raise Exception('error_rco_exp: ' + repr(e))      

    def rco_stmt(self, s: stmt) -> List[stmt]:
        match s: 
            case Assign([Name(x)], e):
                return [Assign([Name(x)], e)] # TODO: do the same thing for assign, ending with an Assign stmt(very similar)
            case Expr(e):
                simpl_e, temps = self.rco_exp(e, False)
                assigned_temps = [Assign([t], e_sub) for (t, e_sub) in temps]
                return assigned_temps + [Expr(simpl_e)]
            case _:
                raise Exception('error_rco_stmt: ' + repr(s))

    def remove_complex_operands(self, p: Module) -> Module:
        match p:
            case Module(body):
                finalBody = []
                body_simplified1 = [self.rco_stmt(s) for s in body] # List[List[stmt]]

                # Turn list of list of statements into list of statements
                for body in body_simplified1:
                    for statement in body:
                        finalBody.append(statement)

                return Module(finalBody) # TODO: sum(xs, z) -> x1 + x2 + ... + z
            case _:
                raise Exception('error remove_complex_operands: ' + repr(p))
    
    ############################################################################
    # Select Instructions
    ############################################################################
    
    # The expression e passed to select_arg should furthermore be an atom.
    # (But there is no type for atoms, so the type of e is given as expr.)
    def select_arg(self, e: expr) -> arg:
        match e:
            case Name(x):
                return Variable(x)
            case Constant(n):
                return Immediate(n)
            case _:
                raise Exception('select_arg unhandled: ' + repr(e))

    def select_stmt(self, s: stmt) -> List[instr]:
        match s:
            case Expr(Call(Name('input_int'), [])):
                return [Callq('read_int', 0)]
            case Expr(Call(Name('print'), [operand])):
                return [Instr('movq', [self.select_arg(operand), Reg('rdi')]),
                        Callq('print_int', 1)]
            case Expr(value):
                return []
            case Assign([x], Name(y)):
                arg_x = self.select_arg(x)
                arg_y = self.select_arg(Name(y))
                return [Instr('movq', [arg_y, arg_x])] # x = y => movq y x #TODO: unnecessary code elim
            case Assign([x], Constant(value)):
                arg_x = self.select_arg(x)
                new_val = self.select_arg(Constant(value))
                return [Instr('movq', [new_val, arg_x])]
            case Assign([x], UnaryOp(USub(), Constant(n))):
                arg_x = self.select_arg(x)
                return [Instr('negq', [arg_x]),
                        Instr('movq', [n, arg_x])]
                        
            case Assign([x], UnaryOp(USub(), e)):
                arg_x = self.select_arg(x)
                num = self.select_stmt(e)
                return [Instr('negq', [arg_x]),
                        Instr('movq', [num, arg_x])]

            case Assign([x], BinOp(left, Add(), right)) if x == left:
                arg_x = self.select_arg(x)
                arg_right = self.select_arg(right)
                # x = x + 1 => [addq $0x1 x]
                return [Instr('addq', [arg_right, arg_x])]
            case Assign([x], BinOp(left, Add(), right)) if x == right:
                arg_x = self.select_arg(x)
                arg_left = self.select_arg(left)
                # x = 1 + x => [addq $0x1 x]
                return [Instr('addq', [arg_left, arg_x])]
            case Assign([x], BinOp(left, Add(), right)):
                arg_x = self.select_arg(x)
                arg_left = self.select_arg(left)
                arg_right = self.select_arg(right)
                return [Instr('addq', [arg_left, arg_x]), 
                        Instr('addq', [arg_right, arg_x])]
                # x = y + z => [movq y x; addq z x]

            case Assign([x], BinOp(left, Sub(), right)) if x == left: 
                arg_x = self.select_arg(x)
                arg_right = self.select_arg(right)
                # x = x - arg => [subq arg x]
                return [Instr('subq', [arg_right, arg_x])]
            case Assign([x], BinOp(left, Sub(), right)) if x == right:
                arg_x = self.select_arg(x)
                arg_left = self.select_arg(left)
                # x = arg - x => [negq x; addq arg x]
                return [Instr('negq', [arg_x]),
                        Instr('addq', [arg_left, arg_x])]
            case Assign([x], BinOp(left, Sub(), right)):
                # x = y - z   => [movq y x; subq z x]
                arg_x = self.select_arg(x)
                arg_left = self.select_arg(left)
                arg_right = self.select_arg(right)
                return [Instr('movq', [arg_left, arg_x]),
                        Instr('subq', [arg_right, arg_x])]

            case Assign([x], Call(Name('input_int'), [])):
                arg_x = self.select_arg(x)
                return [Callq('read_int', []), # stores its answer in Reg('rax')
                        Instr('movq', [Reg('rax'), arg_x])] 
            case _:
                raise Exception('error in select_stmt, unknown: ' + repr(set))

    def select_instructions(self, p: Module) -> X86Program:
        match p:
            case Module(body):
                # finalInstructions = []
                instrs_list = [self.select_stmt(s) for s in body] # List[List[instr]]

                # Turn list of list of statements into list of statements
                # for instrs in instrs_list:
                #     for instruction in instrs:
                #         finalInstructions.append(instruction)

                return X86Program(sum(instrs_list, [])) # sum
    
    ############################################################################
    # Assign Homes
    ############################################################################
    
    @staticmethod
    def gen_stack_access(i: int) -> arg:
        return Deref('rbp', -(8 + 8 * i))
    
    def assign_homes_arg(self, a: arg, home: Dict[Variable, arg]) -> arg:
        # YOUR CODE HERE
        pass        

    def assign_homes_instr(self, i: instr,
                           home: Dict[Variable, arg]) -> instr:
        # YOUR CODE HERE
        pass        

    def assign_homes(self, p: X86Program) -> X86Program:
        match p:
            case x86Program(body):
                variables = ... body # TODO: Collect variables
                home = {}
                for i, x in enumerate(variables):
                    home[x] = self.gen_stack_access(i)
                new_body = ... body, self.assign_homes_instr(body, home) # TODO: Swap variables for Derefs
                p = X86Program(new_body)
                p.stack_space = align(8 * len(variables), 16) # Store stack size
    
    ############################################################################
    # Patch Instructions
    ############################################################################
    '''
    def in_memory(self, a: arg) -> bool:
        pass # return true if the arg is a Deref. isInstance(...)

    def big_number(self, a: arg) -> bool:
        pass # return true if the arg is an Immediate and doesn't fit in an int32

    def patch_instr(self, i: instr) -> List[instr]:
        # YOUR CODE HERE
             
        # check if the arg to the instruction are both memory or one memory + one big immediate
        #   replace one of the memory or the big immediate with a movq ..., %rax
        #   Reg('rax')
        match i:
            case Instr('movq', [x,y]):
                # ... if x and y were both memory references
                #     [move x int %rax,
                #      move %rax into y]
                pass

    def patch_instructions(self, p: X86Program) -> X86Program:
        # YOUR CODE HERE
        pass
        # return a new x86Program after list-comprehending every instruction (flatten)        
    
    ############################################################################
    # Prelude & Conclusion
    ############################################################################
    
    def prelude_and_conclusion(self, p: X86Program) -> X86Program:
        # YOUR CODE HERE
        match p:
            case X86Program(body):
                prelude = [..., #pushq %rbp
                           ..., #movq %rsp %rbp
                           ...,]#subq p.stack_space %rsp
                conclusion = [..., #addq p.stack_space %rsp
                              ..., #popq %rbp
                              ...,]#retq
                return x86Program(prelude + body + conclusion)        
    '''


typecheck_Lvar = type_check_Lvar.TypeCheckLvar().type_check
typecheck_dict = {
    'source': typecheck_Lvar,
    'partial_eval': typecheck_Lvar,
    'remove_complex_operands': typecheck_Lvar,
}
interpLvar = interp_Lvar.InterpLvar().interp
interp_dict = {
    'partial_eval': interpLvar,
    'remove_complex_operands': interpLvar,
    'select_instructions': interp_x86,
    'assign_homes': interp_x86,
    'patch_instructions': interp_x86,
}

