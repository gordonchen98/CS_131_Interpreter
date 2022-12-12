from copy import copy, deepcopy
from intbase import InterpreterBase, ErrorType
from scope import find_variable, find_variable_or_function
from variable import Function, Variable
import operator
import shlex

Scope_Stack = list[dict[str, Variable]]


class Interpreter(InterpreterBase):
    def __init__(self, console_output=True, input=None, trace_output=False):
        # call InterpreterBase’s constructor
        super().__init__(console_output, input)

        # Member variables for setup
        self.tokenize_program: list[list[str]] = []
        self.indentation: list[int] = []

        # Member variables for control structure
        self.call_stack: list[tuple[int, Scope_Stack]] = [(0, [{}])]
        self.scope_stack: Scope_Stack = [{}]
        self.while_table: dict[int, int] = {}
        self.return_type: dict[int, int] = {}
        self.ip: int = 0
        self.terminate: bool = False

        # Member variables for memories
        # instruction pointer of all functions
        self.functions: dict[str, Function] = {self.FUNC_DEF: Function()}

    #################################################################
    ################## Variables helper functions ###################
    #################################################################
    def generate_variable(self, token: str) -> Variable:
        """generate variable according to token. Primitives and Functions will 
        return a new Variable object, and variable and object in the scope will 
        return the object reference to the variable in the scope

        Args:
            token (str): int, string, bool, or variable name

        Returns:
            Variable: Return a new Variable object for primatives or a object
            reference to the Variable Object with given variable name
        """
        # token is a True/False constant
        type = value = None
        if token == self.TRUE_DEF or token == self.FALSE_DEF:
            type = self.BOOL_DEF
            value = True if token == self.TRUE_DEF else False
        # token is a variable or function or object, return the variable directly
        elif token[0].isalpha():
            error, variable = find_variable_or_function(
                token, self.scope_stack, self.functions)
            if variable is None:
                if error == "name":
                    super().error(ErrorType.NAME_ERROR, line_num=self.ip)
                else:
                    super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
            return variable
        # token is a string constant
        elif token[0] == '"':
            type = self.STRING_DEF
            value = token[1:-1]
        # token is a int
        else:
            try:
                type = self.INT_DEF
                value = int(token)
            except ValueError:
                super().error(ErrorType.SYNTAX_ERROR, line_num=self.ip)
        return Variable(type, value)

    def process_expression(self, expression: list[str]) -> Variable:
        """Evaluate the given expression with prefix notation

        Args:
            expression (list[str]): expression list for us to process

        Returns:
            Variable: Variable object reference if expression has only a 
            variable name; otherwise, new Variable object whose value and type 
            is the result of the given expression
        """
        operators = {'+', '-', '*', '/', '%', '<',
                     '>', '<=', '>=', '!=', '==', '&', '|'}
        arithmetic_operators = {
            '+':  operator.add,
            '-':  operator.sub,
            '*':  operator.mul,
            '/':  operator.floordiv,
            '%':  operator.mod,
            '<':  operator.lt,
            '>':  operator.gt,
            '<=': operator.le,
            '>=': operator.ge,
            '!=': operator.ne,
            '==': operator.eq
        }
        string_operators = {
            '+':  operator.concat,
            '<':  operator.lt,
            '>':  operator.gt,
            '<=': operator.le,
            '>=': operator.ge,
            '!=': operator.ne,
            '==': operator.eq
        }
        boolean_operators = {
            '!=': operator.ne,
            '==': operator.eq,
            '&': operator.and_,
            '|': operator.or_
        }
        st = []

        for c in expression[::-1]:
            if c not in operators:
                st.append(self.generate_variable(c))
            else:
                # Sanity check
                if len(st) < 2:
                    super().error(ErrorType.SYNTAX_ERROR, line_num=self.ip)

                type1, o1 = st.pop().get()
                type2, o2 = st.pop().get()

                # Type comparison
                if type1 != type2:
                    super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

                # Type checking
                if type1 not in {self.INT_DEF, self.STRING_DEF, self.BOOL_DEF} \
                        or type2 not in {self.INT_DEF, self.STRING_DEF, self.BOOL_DEF}:
                    super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

                # Operand checking
                if (type1 == self.INT_DEF and c not in arithmetic_operators)       \
                        or (type1 == self.STRING_DEF and c not in string_operators)\
                        or (type1 == self.BOOL_DEF and c not in boolean_operators):
                    super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

                # Expression processing
                value = None
                if type1 == self.INT_DEF:
                    value = arithmetic_operators[c](o1, o2)
                elif type1 == self.STRING_DEF:
                    value = string_operators[c](o1, o2)
                    if c == '+':
                        value = f'"{value}"'
                else:
                    value = boolean_operators[c](o1, o2)
                st.append(self.generate_variable(str(value)))

        return st.pop()

    def handle_var(self, type: str, variables: list[str]) -> int:
        """Handle var keyword, generate all the necessary variables in the 
        current scope. Initialize the variable with the default value.

        Args:
            type (str): type to initialize variables
            variables (list[str]): list of variable names to initialize

        Returns:
            int: next instruction pointer
        """
        default = {
            self.INT_DEF: '0',
            self.BOOL_DEF: 'False',
            self.STRING_DEF: '""',
            self.FUNC_DEF: f'{self.FUNC_DEF}',
            self.OBJECT_DEF: f'{self.OBJECT_DEF}'
        }

        # Type check
        if type not in default:
            super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

        for variable in variables:
            # Redefinition check
            if variable in self.scope_stack[-1]:
                super().error(ErrorType.NAME_ERROR, line_num=self.ip)
            self.handle_assign(variable, [default[type]], True)
        return self.ip + 1

    def handle_assign(self, name: str, expression: list[str], flag: bool = False) -> int:
        """Handle any assignment of variables with the given expression.

        Args:
            name (str): name of the variable
            expression (list[str]): expression we want to change the value of 
            the variable to
            flag (bool, optional): True if calling from handle_var. Defaults to False.

        Returns:
            int: next instruction pointer
        """
        value_type, value = self.process_expression(expression).get()

        if flag:
            self.scope_stack[-1][name] = Variable(value_type, value)
            return self.ip

        error, variable = find_variable(name, self.scope_stack)
        if variable is None:
            if error == "type":
                super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
            else:
                super().error(ErrorType.NAME_ERROR, line_num=self.ip)

        # deal with object member variable assignement
        if len(name.split('.')) == 2:
            if value_type == self.OBJECT_DEF:
                variable.type, variable.value = value_type, value
            else:
                variable.type, variable.value = value_type, deepcopy(value)
        else:
            variable_type, _ = variable.get()
            if value_type != variable_type:
                super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
            variable.value = value

        return self.ip + 1

    def handle_return_assign(self, expression: list[str], builtin: bool = False):
        """handle all the resulti/b/s/f/o assignment according to the expression.
        Expression can be any expression as defined in the syntax.

        Args:
            expression (list[str]): expression to return
            builtin (bool, optional): True if return from built-in function.
            Defaults to False.
        """
        type, value = self.process_expression(expression).get()

        if builtin:
            if type == self.STRING_DEF:
                self.scope_stack[0]['results'] = Variable(type, value)
            elif type == self.INT_DEF:
                self.scope_stack[0]['resulti'] = Variable(type, value)
            return

        func_type = self.tokenize_program[self.return_type[self.ip]][-1]
        # Check if the return type matches the function return signature
        if type != func_type:
            super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

        variable = Variable(type, value)
        top_level_scope_calling_func = self.call_stack[-1][1][0]
        top_level_scope_calling_func[f'result{func_type[0]}'] = variable

    #################################################################
    ################### Functions helper functions ##################
    #################################################################
    def handle_builtIn_func(self, name: str, expression: list[str]) -> int:
        if name == self.INPUT_DEF or name == self.PRINT_DEF:
            res = []
            for token in expression:
                _, value = self.generate_variable(token).get()
                res.append(str(value))
            super().output(''.join(res))
            if name == self.INPUT_DEF:
                input = super().get_input()
                self.handle_return_assign([f'"{input}"'], True)
        else:
            # sanity check
            if len(expression) != 1:
                super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

            # tokens[1] is variable or constant
            type, value = self.generate_variable(expression[0]).get()
            if type != self.STRING_DEF:
                super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

            # Check if the string can be converted to int
            try:
                self.handle_return_assign(
                    [str(int(value))], True)  # type: ignore
            except ValueError:
                super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

        return self.ip + 1

    def handle_funccall(self, tokens: list[str]) -> int:
        name, arguments = tokens[0], tokens[1:]
        if name in {self.PRINT_DEF, self.STRTOINT_DEF, self.INPUT_DEF}:
            expression = tokens[1:]
            return self.handle_builtIn_func(name, expression)

        # get the funccall
        error, variable = find_variable_or_function(
            name, self.scope_stack, self.functions)
        if variable is None:
            if error == "type":
                super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
            else:
                super().error(ErrorType.NAME_ERROR, line_num=self.ip)

        type, function = variable.get()
        if type != self.FUNC_DEF:
            super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
        func_ip = function.ip  # type: ignore

        # Constructing the parameter passing to func call
        if self.tokenize_program[func_ip][0] == self.FUNC_DEF:
            func_signatures = self.tokenize_program[func_ip][2:-1]
        else:
            func_signatures = self.tokenize_program[func_ip][1:-1]

        # Grab out the closure of the Function object
        closure: list[dict[str, Variable]] = [
            {}] if function.closure is None else deepcopy(function.closure)  # type: ignore
        param_passing: dict[str, Variable] = {}

        # Check if there is correct number of arguments with funccall
        if len(func_signatures) != len(arguments):
            super().error(ErrorType.NAME_ERROR, line_num=self.ip)

        for argument, signature in zip(arguments, func_signatures):
            param_name, type = signature.split(':')
            variable = self.process_expression([argument])
            if type in {self.REFBOOL_DEF, self.REFINT_DEF, self.REFSTRING_DEF, self.OBJECT_DEF}:
                if type[3:] != variable.type and self.OBJECT_DEF != variable.type:
                    super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
            else:
                if type != variable.type:
                    super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
                variable = Variable(variable.type, variable.value)

            # Redefinition within parameter
            if param_name in param_passing:
                super().error(ErrorType.NAME_ERROR, line_num=func_ip)
            param_passing[param_name] = variable

        # If we call function from method member variable, create a this -> obj
        # key-val pair in the scope for the funccall
        name = name.split('.')
        if len(name) == 2:
            error, obj = find_variable_or_function(
                name[0], self.scope_stack, self.functions)
            if obj is None:
                if error == "type":
                    super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
                else:
                    super().error(ErrorType.NAME_ERROR, line_num=self.ip)
            param_passing[self.THIS_DEF] = obj

        closure.append(param_passing)  # type: ignore

        self.call_stack.append((self.ip + 1, self.scope_stack))
        self.scope_stack = copy(closure)
        return func_ip + 1

    def handle_return(self, expression: list[str]) -> tuple[int, Scope_Stack]:
        default = {
            self.INT_DEF: '0',
            self.BOOL_DEF: 'False',
            self.STRING_DEF: '""',
            self.FUNC_DEF: 'resultf',
            self.OBJECT_DEF: f'{self.OBJECT_DEF}'
        }

        func_ip = self.return_type[self.ip]
        func_type = self.tokenize_program[func_ip][-1]

        # if function return type is void, dun care
        if func_type == self.VOID_DEF:
            if expression:
                super().error(ErrorType.TYPE_ERROR, line_num=self.ip)
            self.scope_stack.pop()
            return self.call_stack.pop()

        if func_type not in default:
            super().error(ErrorType.SYNTAX_ERROR, line_num=func_ip)

        if not expression:
            expression = [default[func_type]]

        self.handle_return_assign(expression)
        self.scope_stack.pop()

        return self.call_stack.pop()

    def handle_lambda(self) -> int:
        function = Function(self.ip, deepcopy(self.scope_stack))
        self.scope_stack[0]['resultf'] = Variable(self.FUNC_DEF, function)

        ip, indent = self.ip, self.indentation[self.ip]
        while self.tokenize_program[ip][0] != self.ENDLAMBDA_DEF or self.indentation[ip] != indent:
            ip += 1

        return ip + 1

    #################################################################
    ############### Control structure helper function ###############
    #################################################################
    def handle_while(self, expression: list[str]) -> int:
        type, value = self.process_expression(expression).get()
        if type != self.BOOL_DEF:
            super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

        ip = self.ip
        if not value:
            ip = self.while_table[ip]
        else:
            self.scope_stack.append({})

        return ip + 1

    def handle_if(self, expression: list[str]) -> int:
        type, value = self.process_expression(expression).get()
        if type != self.BOOL_DEF:
            super().error(ErrorType.TYPE_ERROR, line_num=self.ip)

        self.scope_stack.append({})

        if not value:
            for line_num in range(self.ip, len(self.tokenize_program)):
                tokens = self.tokenize_program[line_num]
                if not tokens:
                    continue
                if tokens[0] == self.ELSE_DEF and \
                        self.indentation[self.ip] == self.indentation[line_num]:
                    return line_num + 1
                if tokens[0] == self.ENDIF_DEF and \
                        self.indentation[self.ip] == self.indentation[line_num]:
                    return line_num

        return self.ip + 1

    def handle_else(self) -> int:
        for line_num in range(self.ip+1, len(self.tokenize_program)):
            tokens = self.tokenize_program[line_num]
            if not tokens:
                continue
            if tokens[0] == self.ENDIF_DEF and \
                    self.indentation[self.ip] == self.indentation[line_num]:
                return line_num
        return len(self.tokenize_program) - 1

    #################################################################
    ################# Interpreter helper functions ##################
    #################################################################

    def tokenize(self, program: list[str]) -> None:
        for line in program:
            self.indentation.append(len(line) - len(line.lstrip()))

            tokenized_line = shlex.split(line, comments=True, posix=False)
            self.tokenize_program.append(
                tokenized_line if tokenized_line else [''])

    def construct_jmp_table(self) -> None:
        st = []
        for i, line_token in enumerate(self.tokenize_program):
            if line_token[0] == self.FUNC_DEF:
                self.functions[line_token[1]] = Function(i)

            if line_token[0] == self.WHILE_DEF:
                st.append(i)

            if line_token[0] == self.ENDWHILE_DEF:
                last = st.pop()
                self.while_table[i] = last
                self.while_table[last] = i

    def construct_return_type_list(self) -> None:
        st = []
        for i, line_token in enumerate(self.tokenize_program):
            if line_token[0] == self.FUNC_DEF or line_token[0] == self.LAMBDA_DEF:
                st.append(i)

            if line_token[0] == self.RETURN_DEF:
                if not st:
                    super().error(ErrorType.SYNTAX_ERROR, i)
                self.return_type[i] = st[-1]

            if line_token[0] == self.ENDFUNC_DEF or line_token[0] == self.ENDLAMBDA_DEF:
                if not st:
                    super().error(ErrorType.SYNTAX_ERROR, i)
                self.return_type[i] = st.pop()

    def interpret(self) -> None:
        tokenize_line = self.tokenize_program[self.ip]

        if tokenize_line[0] == self.FUNC_DEF:
            self.ip += 1

        elif tokenize_line[0] == self.ENDFUNC_DEF:
            self.ip, self.scope_stack = self.handle_return([])

            if not self.call_stack:
                self.terminate = True

        elif tokenize_line[0] == self.WHILE_DEF:
            expression = tokenize_line[1:]
            self.ip = self.handle_while(expression)

        elif tokenize_line[0] == self.ENDWHILE_DEF:
            self.scope_stack.pop()
            self.ip = self.while_table[self.ip]

        elif tokenize_line[0] == self.IF_DEF:
            expression = tokenize_line[1:]
            self.ip = self.handle_if(expression)

        elif tokenize_line[0] == self.ELSE_DEF:
            self.ip = self.handle_else()

        elif tokenize_line[0] == self.ENDIF_DEF:
            self.scope_stack.pop()
            self.ip += 1

        elif tokenize_line[0] == self.ASSIGN_DEF:
            self.ip = self.handle_assign(tokenize_line[1], tokenize_line[2:])

        elif tokenize_line[0] == self.FUNCCALL_DEF:
            self.ip = self.handle_funccall(tokenize_line[1:])

        elif tokenize_line[0] == self.RETURN_DEF:
            self.ip, self.scope_stack = self.handle_return(tokenize_line[1:])

            if not self.call_stack:
                self.terminate = True

        elif tokenize_line[0] == self.VAR_DEF:
            self.ip = self.handle_var(tokenize_line[1], tokenize_line[2:])

        elif tokenize_line[0] == self.LAMBDA_DEF:
            self.ip = self.handle_lambda()

        elif tokenize_line[0] == self.ENDLAMBDA_DEF:
            self.ip, self.scope_stack = self.handle_return(tokenize_line[1:])
            if not self.call_stack:
                self.terminate = True

        else:
            self.ip += 1

    def run(self, program: list[str]) -> None:
        self.tokenize(program)
        self.construct_jmp_table()
        self.construct_return_type_list()

        # Find ip for main
        if 'main' not in self.functions:
            super().error(ErrorType.SYNTAX_ERROR)
        else:
            self.ip = self.functions['main'].ip

        while not self.terminate:
            self.interpret()
