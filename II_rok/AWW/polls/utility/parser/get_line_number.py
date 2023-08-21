import re
from polls.utility.parser.regex_constant import line_number, one_new_line


def get_line_number(file_name, string):
    regex = file_name + line_number

    match = re.search(regex, string)
    if match:
        value = int(match.group(1))
        what = match.group(2)
        return value, what, string


def line_numbers(filepath, file_name):
    resoult = []

    with open(filepath, 'r') as file:
        for line in file:
            resoult.append(get_line_number(file_name, line))
    return resoult


def get_tet_position(filepath, regex_pattern: str):

    regex_pattern = regex_pattern.replace("\r\n", "\n")
    # regex_pattern = regex_pattern.replace("\t", "")
    regex_pattern = re.escape(regex_pattern)
    # regex_pattern = re.sub(one_new_line, '\n', regex_pattern)

    with open(filepath, 'r') as file:
        file_contents = file.read()
        # print(file_contents)
        regex_matches = re.finditer(regex_pattern, file_contents)

        match = next(regex_matches, None)
        # print(match)
        if match:
            match_start = match.start()
            match_end = match.end()

            line_start = file_contents.count('\n', 0, match_start) + 1
            line_end = file_contents.count('\n', 0, match_end) + 1
            # print(f"start {line_start} end {line_end}")
            return line_start, line_end
        else:
            return None