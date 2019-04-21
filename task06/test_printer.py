from printer import *
from model import *
import pytest


def test_cond():
    a = PrettyPrinter()
    result = a.visit_conditional(Conditional(Number(42), [], []))
    assert result == "if (42) {\n}\n"


def test_fdef():
    a = PrettyPrinter()
    result = a.visit_function_definition(FunctionDefinition("foo",
                                                            Function([], [])))
    assert result == "def foo() {\n}\n"


def test_print():
    a = PrettyPrinter()
    result = a.visit_print(Print(Number(42)))
    assert result == "print 42;\n"


def test_read():
    a = PrettyPrinter()
    result = a.visit_read(Read("x"))
    assert result == "read x;\n"


def test_number():
    a = PrettyPrinter()
    result = a.visit_number(Number(10))
    assert result == "10;\n"


def test_ref():
    a = PrettyPrinter()
    result = a.visit_reference(Reference("x"))
    assert result == "x;\n"


def test_bin_op():
    a = PrettyPrinter()
    add = BinaryOperation(Number(2), "+", Number(3))
    mul = BinaryOperation(Number(1), "*", add)
    result = a.visit_binary_operation(mul)
    assert result == "(1 * (2 + 3));\n"


def test_un_op():
    a = PrettyPrinter()
    result = a.visit_unary_operation(UnaryOperation("-", Number(42)))
    assert result == "(-42);\n"


def test_fcall():
    a = PrettyPrinter()
    result = a.visit_function_call(FunctionCall(Reference("foo"),
                                                [Number(1), Number(2),
                                                 Number(3)]))
    assert result == "foo(1, 2, 3);\n"


def test_pretty_print(capsys):
    fcall = FunctionCall(Reference("foo"), [Reference("x")])
    cond = Conditional(BinaryOperation(Reference("x"), ">", Number(0)),
                       [fcall, Print(Reference("x"))],
                       [BinaryOperation(Reference("x"), "+", Number(121))])
    pretty_print(cond)
    out, err = capsys.readouterr()
    ans = "if ((x > 0)) {\n" \
          "\tfoo(x);\n" \
          "\tprint x;\n" \
          "} else {\n" \
          "\t(x + 121);\n" \
          "}\n"
    assert out == ans


def test_pretty_print_2(capsys):
    pretty_print(FunctionDefinition("main", Function(["arg1"], [
        Read("x"),
        Print(Reference("x")),
        Conditional(
            BinaryOperation(Number(2), "==", Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference("exit"), [
                    UnaryOperation("-", Reference("arg1"))
                ])
            ],
        ),
    ])))
    out, err = capsys.readouterr()
    ans = "def main(arg1) {\n" \
          "\tread x;\n" \
          "\tprint x;\n" \
          "\tif ((2 == 3)) {\n" \
          "\t\tif (1) {\n" \
          "\t\t}\n" \
          "\t} else {\n" \
          "\t\texit((-arg1));\n" \
          "\t}\n" \
          "}\n"
    assert out == ans


if __name__ == "__main__":
    pytest.main()
