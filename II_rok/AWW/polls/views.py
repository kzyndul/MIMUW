import os

from django.contrib.auth.decorators import login_required
from django.shortcuts import render, redirect, get_object_or_404
from django.utils import timezone
from django.contrib import auth
from django.contrib import messages
from django.views.defaults import server_error

from polls.utility.parser import my_parser
from polls.models import Directory, File, FileForm, DirectoryForm, FileSection, LoginForm, get_default_section_type, \
    create_default_section_status, ceate_default_section_data
from polls.utility.db import create_sections, assigne_section_status
from polls.utility.parser.get_line_number import get_tet_position
from polls.utility.util import get_context_file, get_context


def loginPage(request):
    if request.method == 'POST':
        form = LoginForm(request.POST)
        if form.is_valid():
            username = form.cleaned_data['username']
            password = form.cleaned_data['password']
            user = auth.authenticate(request, username=username, password=password)
            if user is not None:
                auth.login(request, user)
                messages.success(request, 'You are now logged in')
                return redirect('homePage')
            else:
                messages.error(request, 'Invalid credentials')
                return redirect('login')
    else:
        form = LoginForm()
    return render(request, 'polls/loginPage.html', {'form': form})


@login_required(login_url='login')
def homePage(request):
    context = get_context(request)
    return render(request, 'polls/homePage.html', context)


@login_required(login_url='login')
def getHomePageFile(request, pk):
    if request.method == 'POST':
        form = FileForm(request.POST, request.FILES, initial={'owner': request.user})
        form.fields["owner"].initial = request.user

        if form.is_valid():
            file = form.save()
            create_sections(my_parser(os.path.abspath(file.filePath.path)), file)
            context = get_context(request)
            return render(request, 'polls/homePage.html', context)
        else:
            error_fileds = []
            # for field, errors in form.errors.items():
            #     for error in errors:
            #         print(f"Validation error in field '{field}': {error}")

            for field in form.errors.items():
                error_fileds.append(field[0])
            return render(request, 'polls/errorPage.html', context={"errors": error_fileds})
    else:
        file = File.objects.filter(id=pk).first()
        context = get_context_file(request, file)
        return render(request, 'polls/homePage.html', context)


@login_required(login_url='login')
def getHomePageDirectory(request):
    if request.method == 'POST':
        form = DirectoryForm(request.POST, initial={'owner': request.user})
        form.fields["owner"].initial = request.user
        if form.is_valid():
            directory = form.save()
            context = get_context(request)
            return render(request, 'polls/homePage.html', context)
        else:
            error_fileds = []
            # for field, errors in form.errors.items():
            #     for error in errors:
            #         print(f"Validation error in field '{field}': {error}")

            for field in form.errors.items():
                error_fileds.append(field[0])
            return render(request, 'polls/errorPage.html', context={"errors": error_fileds})
    else:
        context = get_context(request)
        return render(request, 'polls/homePage.html', context)


def deleteAbstract(pk, model):
    my_object = get_object_or_404(model, id=pk)
    my_object.visibility = not my_object.visibility
    my_object.vDate = timezone.now()
    my_object.save()
    return redirect('homePage')


@login_required(login_url='login')
def deleteDirectory(request, pk):
    return deleteAbstract(pk, Directory)


@login_required(login_url='login')
def deleteFile(request, pk):
    return deleteAbstract(pk, File)


@login_required(login_url='login')
def deleteSection(request, pk, flieSectionPK):
    section = FileSection.objects.get(id=flieSectionPK)
    section.delete()
    context = get_context_file(request, File.objects.filter(id=pk).first())
    return render(request, 'polls/homePage.html', context)


@login_required(login_url='login')
def createSection(request, pk):
    selected_text = request.POST.get('selectedTextFileSection', '')
    file = File.objects.get(id=pk)
    category = request.POST.get('sectionTypeForm')
    para = get_tet_position(os.path.abspath(file.filePath.path), selected_text)

    begin = None
    end = None
    if para is not None:
        begin = para[0]
        end = para[1]
    name = request.POST.get('selectFileName')
    description = request.POST.get('selectFileDescription')

    if (file is not None) and (category is not None) and (begin is not None):
        section_type = get_default_section_type(category)
        sectionStatus = create_default_section_status()

        sectionData = ceate_default_section_data()
        fileSectio = FileSection(name=name, description=description, begin=begin, end=end, sectionType=section_type,
                                 file=file, sectionStatus=sectionStatus, sectionData=sectionData)
        fileSectio.save()

        context = get_context_file(request, file)
        return render(request, 'polls/homePage.html', context)
    else:
        context = {'text': "zaznacz poprawna sekcję"}
        return render(request, 'polls/errorPage.html', context)


@login_required(login_url='login')
def compileFile(request, pk):
    assigne_section_status(request, pk)
    context = get_context_file(request, File.objects.filter(id=pk).first())
    return render(request, 'polls/homePage.html', context)
