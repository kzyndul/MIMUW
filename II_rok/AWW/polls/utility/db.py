import os
import shutil
import subprocess
import tempfile

from polls.models import FileSection, get_default_section_type, get_default_section_status, get_default_section_data, \
    SectionData, File
from polls.utility import get_options, get_section
from polls.utility.parser import line_numbers


def create_sections(section_list, file):
    for x in section_list:
        sectionT = get_default_section_type(x.type)
        sectionS = get_default_section_status(None)
        sectionD = get_default_section_data()
        file_section = FileSection(begin=x.start, sectionData=sectionD, sectionStatus=sectionS, sectionType=sectionT, end=x.end, file=file)
        file_section.save()

def assigne_section_status(request, pk):
    file = File.objects.get(id=pk)
    file_path = file.filePath.path
    with tempfile.TemporaryDirectory() as tmp_folder:
        new_file_path = shutil.copy(file_path, tmp_folder)
        output_file_name = 'output.txt'

        options = "-o" + tmp_folder + "/" + get_options(request)
        # print(options)
        out = subprocess.run(['sdcc', options, new_file_path], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        output_file_path = os.path.join(tmp_folder, output_file_name)

        open(output_file_path, 'w').close()
        with open(output_file_path, 'w') as f:
            f.write(out.stderr.decode())

        errors = line_numbers(output_file_path, new_file_path)
        sections = get_section(File.objects.get(id=pk))

    for section in sections:
        section.sectionStatus = get_default_section_status("KOMPILUJE")
        for x in errors:
            if x is not None:
                if section.begin <= x[0] <= section.end and section.sectionType.category != "komentarz":
                    section.sectionStatus = get_default_section_status(x[1])
                    a = SectionData(line_number=x[0], compilatorError=x[2].replace("\n", ""))
                    a.save()
                    section.sectionData = a
        section.save()


