from polls.utility.parser.regex_constant import *
from polls.utility.parser.section import Section, Number, increment
import re


def my_parser(filepath):
    resoult = []

    line_number = Number(1)
    line_number_local = 1
    with open(filepath, 'r') as file:
        resoult.extend(comments(filepath))


        for line in file:



            if re.search(block_comment_start, line):
                while not re.search(block_comment_end, line):
                    increment(line_number)
                    line_number_local += 1
                    line = next(file, None)


            if re.search(function, line):
                resoult.append(function_matched(line_number.value, filepath))

            if re.search(assembler_start, line):
                resoult.append(assembler(line_number.value, filepath))

            if re.search(block_compiler_directive_start, line):
                resoult.append(compiler_directive_block(line_number.value, filepath))


            if re.search(line_compiler_directive, line):
                resoult.append(Section(line_number.value, line_number.value, "dyrektywny kompilatora"))

            if re.search(variable, line):
                resoult.append(Section(line_number.value, line_number.value, "deklaraca zmiennej"))



            line_number_local += 1
            increment(line_number)
    return resoult


def comments(filepath):
    resoult = []
    line_number = Number(1)
    with open(filepath, 'r') as file:
        for line in file:

            if re.search(block_comment_start, line):
                tmp_start = line_number.value
                while not re.search(block_comment_end, line):
                    increment(line_number)
                    line = next(file, None)
                resoult.append(Section(tmp_start, line_number.value, "komentarz"))

            if re.search(line_comment, line):
                resoult.append(Section(line_number.value, line_number.value, "komentarz"))

            increment(line_number)
    return resoult

def function_matched(line_number, filepath):
    tmp_start = 1
    m_number_of_open = 0
    m_number_of_close = 0


    with open(filepath, 'r') as file:
        line = next(file, None)
        while tmp_start != line_number:
            line = next(file, None)
            tmp_start += 1

        while m_number_of_open > m_number_of_close or m_number_of_open == 0:


            m_open = re.findall(curly_braces_open, line)
            m_number_of_open += len(m_open)
            m_close = re.findall(curly_braces_close, line)
            m_number_of_close += len(m_close)
            line = next(file, None)
            line_number += 1

    return Section(tmp_start, line_number - 1, "procedura")

def compiler_directive_block(line_number, filepath):
    tmp_start = 1
    with open(filepath, 'r') as file:
        line = next(file, None)
        while tmp_start != line_number:
            line = next(file, None)
            tmp_start += 1


        while not re.search(block_compiler_directive_end, line):


            line = next(file, None)
            line_number += 1


    return Section(tmp_start, line_number, "dyrektywny kompilatora")


def assembler(line_number, filepath):
    tmp_start = 1
    with open(filepath, 'r') as file:
        line = next(file, None)
        while tmp_start != line_number:
            line = next(file, None)
            tmp_start += 1


        while not re.search(assembler_end, line):

            line_number += 1
            line = next(file, None)

    return Section(tmp_start, line_number, "wstawka asemblerowa")
