import model


class PrettyPrinter(model.ASTNodeVisitor):
    def __init__(self):
        self.number_of_tabs = 0
        self.is_instruction = True

    def visit_number(self, number):
        if self.is_instruction:
            result = "\t" * self.number_of_tabs
        else:
            result = ""
        result += str(number.value)
        if self.is_instruction:
            result += ";\n"
        return result

    def visit_conditional(self, conditional):
        result = "\t" * self.number_of_tabs + "if ("
        result += self.format_(conditional.condition, is_instruction=False)
        result += ") {\n"
        if conditional.if_true:
            self.number_of_tabs += 1
            for exp in conditional.if_true:
                result += self.format_(exp)
            self.number_of_tabs -= 1
        result += "\t" * self.number_of_tabs + "}"
        if conditional.if_false:
            result += " else {" + "\n"
            self.number_of_tabs += 1
            for exp in conditional.if_false:
                result += self.format_(exp)
            self.number_of_tabs -= 1
            result += "\t" * self.number_of_tabs + "}"
        result += "\n"
        return result

    def visit_print(self, printt):
        result = "\t" * self.number_of_tabs + "print "
        result += self.format_(printt.expr, is_instruction=False)
        result += ";\n"
        return result

    def visit_read(self, read):
        result = "\t" * self.number_of_tabs + \
                  "read " + str(read.name)
        result += ";\n"
        return result

    def visit_reference(self, reference):
        if self.is_instruction:
            result = "\t" * self.number_of_tabs
        else:
            result = ""
        result += reference.name
        if self.is_instruction:
            result += ";\n"
        return result

    def visit_function_definition(self, functiondefinition):
        result = "\t" * self.number_of_tabs + \
                  "def " + functiondefinition.name
        result += "("
        for elem in functiondefinition.function.args[0:-1]:
            result += elem + ', '
        if functiondefinition.function.args:
            result += functiondefinition.function.args[-1]
        result += ") {\n"
        self.number_of_tabs += 1
        for exp in functiondefinition.function.body:
            result += self.format_(exp)
        self.number_of_tabs -= 1
        result += "\t" * self.number_of_tabs + "}\n"
        return result

    def visit_function_call(self, functioncall):
        if self.is_instruction:
            result = "\t" * self.number_of_tabs
        else:
            result = ""
        result += self.format_(functioncall.fun_expr, is_instruction=False)
        result += "("
        for elem in functioncall.args[0:-1]:
            result += self.format_(elem, is_instruction=False)
            result += ", "
        if functioncall.args:
            result += self.format_(functioncall.args[-1], is_instruction=False)
        result += ")"
        if self.is_instruction:
            result += ";\n"
        return result

    def visit_binary_operation(self, binaryoperation):
        if self.is_instruction:
            result = "\t" * self.number_of_tabs
        else:
            result = ""
        result += "("
        result += self.format_(binaryoperation.lhs, is_instruction=False)
        result += " " + binaryoperation.op + " "
        result += self.format_(binaryoperation.rhs, is_instruction=False)
        result += ")"
        if self.is_instruction:
            result += ";\n"
        return result

    def visit_unary_operation(self, unaryoperation):
        if self.is_instruction:
            result = "\t" * self.number_of_tabs
        else:
            result = ""
        result += "(" + unaryoperation.op
        result += self.format_(unaryoperation.expr, is_instruction=False)
        result += ")"
        if self.is_instruction:
            result += ";\n"
        return result

    def format_(self, program, *args, is_instruction=True):
        instr = self.is_instruction
        self.is_instruction = is_instruction
        result = program.accept(self)
        self.is_instruction = instr
        return result


def pretty_print(program):
    printer = PrettyPrinter()
    print(printer.format_(program, is_instruction=True), end="")
