import os

from polls.models import FileSection, FileForm, DirectoryForm, Directory, File
from polls.utility.procesor_dependent_options import procesor_dependent_options

def get_context(request):
    resoult = {}

    fileForm = FileForm()
    fileForm.fields["owner"].initial = request.user

    directoryForm = DirectoryForm()
    directoryForm.fields["owner"].initial = request.user

    db_files = get_files(request.user)

    resoult['fileForm'] = fileForm
    resoult['directoryForm'] = directoryForm
    resoult['db_files'] = db_files
    return resoult


def get_context_file(request, file):
    resoult = get_context(request)

    file_line = []
    with open(os.path.abspath(file.filePath.path), 'r') as f:
        for line in f:
            # if line != '\n':
            line = line.replace('\n', '')
            file_line.append(line)
    resoult['file'] = file_line
    resoult['sectionList'] = get_section(file)
    resoult['file_index'] = file.id
    return resoult


def get_section(file):
    return FileSection.objects.filter(file=file).all()

def get_options(request):
    result = ""
    procesor = None
    if "c_language" in request.POST:
        result += " --std-" + str(request.POST['c_language'])



    if "optimize" in request.POST:
        for x in request.POST.getlist('optimize'):
            result += " " + str(x)
            

    if "procesor" in request.POST:
        result += " -m" + str(request.POST['procesor'])
        procesor = request.POST['procesor']



    if procesor is not None and "dependent" in request.POST:
        for x in request.POST.getlist('dependent'):
            if x in procesor_dependent_options[procesor]:
                result += " " + str(x)
    return result


def get_files(user):
    directories = Directory.objects.filter(owner=user).all()
    files = File.objects.filter(owner=user).all()
    directory_files_array = []
    # if directories.:
    if directories.exists():
        for directory in directories:

            files_for_directory = []

            for file in files.filter(directory=directory):
                files_for_directory.append(file)

            directory_files_array.append([directory, files_for_directory])
    return directory_files_array
