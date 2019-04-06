from printer import *
from model import *
import pytest


def test_cond(capsys):
    a = PrettyPrinter()
    a.visit_conditional(Conditional(Number(42), [], []))
    out, err = capsys.readouterr()
    assert out == "if (42) {\n}\n"


def test_fdef(capsys):
    a = PrettyPrinter()
    a.visit_functiondefinition(FunctionDefinition("foo", Function([], [])))
    out, err = capsys.readouterr()
    assert out == "def foo() {\n}\n"


def test_print(capsys):
    a = PrettyPrinter()
    a.visit_print(Print(Number(42)))
    out, err = capsys.readouterr()
    assert out == "print 42;\n"


def test_read(capsys):
    a = PrettyPrinter()
    a.visit_read(Read('x'))
    out, err = capsys.readouterr()
    assert out == "read x;\n"


def test_number(capsys):
    a = PrettyPrinter()
    a.visit_number(Number(10))
    out, err = capsys.readouterr()
    assert out == "10;\n"


def test_ref(capsys):
    a = PrettyPrinter()
    a.visit_reference(Reference('x'))
    out, err = capsys.readouterr()
    assert out == "x;\n"


def test_bin_op(capsys):
    a = PrettyPrinter()
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(1), '*', add)
    a.visit_binaryoperation(mul)
    out, err = capsys.readouterr()
    assert out == "(1 * (2 + 3));\n"


def test_un_op(capsys):
    a = PrettyPrinter()
    a.visit_unaryoperation(UnaryOperation('-', Number(42)))
    out, err = capsys.readouterr()
    assert out == "(-(42));\n"


def test_fcall(capsys):
    a = PrettyPrinter()
    a.visit_functioncall(FunctionCall(Reference('foo'), [Number(1), Number(2), Number(3)]))
    out, err = capsys.readouterr()
    assert out == "foo(1, 2, 3);\n"


def test_pretty_print(capsys):
    fcall = FunctionCall(Reference("foo"), [Reference("x")])
    cond = Conditional(BinaryOperation(Reference("x"), ">", Number(0)),
                       [fcall, Print(Reference("x"))],
                       [BinaryOperation(Reference("x"), "+", Number(121))])
    pretty_print(cond)
    out, err = capsys.readouterr()
    ans = "if ((x > 0)) {\n\tfoo(x);\n\tprint x;\n} else {\n\t(x + 121);\n}\n"
    assert out == ans


def test_pretty_print_2(capsys):
    pretty_print(FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
            ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                ])
            ],
        ),
    ])))
    out, err = capsys.readouterr()
    ans = ("def main(arg1) {\n\tread x;\n\tprint x;"
           "\n\tif ((2 == 3)) {\n\t\tif (1) {\n\t\t}"
           "\n\t} else {\n\t\texit((-(arg1)));\n\t}\n}\n")
    assert out == ans


if __name__ == "__main__":
    pytest.main()
