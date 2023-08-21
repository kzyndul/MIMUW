from django.db import models, connection
from django.contrib.auth.models import User
from django.forms import ModelForm, DateTimeField, BooleanField, IntegerField, HiddenInput, ChoiceField, \
    ModelChoiceField

from django import forms

from django.utils import timezone


def get_default_section_status(string):
    obj = SectionStatus.objects.filter(status=string).first()
    if obj is None and string is not None:
        obj = SectionStatus(status=string)
        obj.save()
    elif obj is None:
        obj = SectionStatus()
        obj.save()
    return obj


def create_default_section_status():
    return SectionStatus.objects.create(status=None)

def section_status_getID():
    return create_default_section_status().pk

def get_default_user():
    obj = User.objects.filter(pk=1).first()
    if obj is  None:
        obj = User(username="admin", password="admin", email="kz438842@students.mimuw.edu.pl")
        obj.save()
    return obj.pk

def get_default_section_type(string):
    obj = SectionType.objects.filter(category=string).first()
    if obj is None:
        obj = SectionType(category=string)
        obj.save()
    return obj

def ceate_default_section_data():
    return SectionData.objects.create()


def section_data_getID():
    return ceate_default_section_data().pk

def get_default_section_data():
    return SectionData.objects.create()



class LoginForm(forms.Form):
    username = forms.CharField(label='Username')
    password = forms.CharField(label='Password', widget=forms.PasswordInput)


class Directory(models.Model):
    name = models.CharField(max_length=200, blank=False, null=False)
    description = models.CharField(max_length=200, default=None, blank=True, null=True)
    visibility = models.BooleanField(default=True)

    cDate = models.DateTimeField("creation date", default=None)
    vDate = models.DateTimeField("visibility change date", default=None)
    change_date = models.DateTimeField("change date", default=None)

    owner = models.ForeignKey(User, on_delete=models.CASCADE, default=get_default_user)

    def __str__(self):
        return f"{self.name} {self.description}"


class DirectoryForm(ModelForm):
    cDate = DateTimeField(initial=timezone.now(), widget=HiddenInput())
    visibility = BooleanField(initial=True)
    vDate = DateTimeField(initial=timezone.now(), widget=HiddenInput())
    change_date = DateTimeField(initial=timezone.now(), widget=HiddenInput())

    owner = ModelChoiceField(queryset=User.objects.all(), widget=HiddenInput())

    class Meta:
        model = Directory
        fields = ['name', 'description', 'cDate', 'visibility', 'vDate', 'change_date', 'owner']


class DeleteDirectoryForm(ModelForm):
    id = IntegerField()

    class Meta:
        model = Directory
        fields = ['id']


class File(models.Model):
    name = models.CharField(max_length=200)
    description = models.CharField(max_length=200, default=None, blank=True, null=True)
    visibility = models.BooleanField(default=True)

    cDate = models.DateTimeField("creation date", default=None)
    vDate = models.DateTimeField("visibility change date", default=None)
    change_date = models.DateTimeField("change date", default=None)

    directory = models.ForeignKey(Directory, on_delete=models.CASCADE, default=None)
    filePath = models.FileField(default=None, upload_to='uploads/')

    owner = models.ForeignKey(User, on_delete=models.CASCADE, default=get_default_user)

    def __str__(self):
        return self.name


class FileForm(ModelForm):
    cDate = DateTimeField(initial=timezone.now(), widget=HiddenInput())
    visibility = BooleanField(initial=True)
    vDate = DateTimeField(initial=timezone.now(), widget=HiddenInput())
    change_date = DateTimeField(initial=timezone.now(), widget=HiddenInput())

    owner = ModelChoiceField(queryset=User.objects.all(), widget=HiddenInput())


    class Meta:
        model = File
        fields = ['name', 'description', 'filePath', 'directory', 'cDate', 'visibility', 'vDate', 'change_date', 'owner']


class DeleteFileForm(ModelForm):
    id = IntegerField()

    class Meta:
        model = File
        fields = ['id']


class SectionType(models.Model):
    categories = (
        ("procedura", "procedura"),
        ("komentarz", "komentarz"),
        ("dyrektywny kompilatora", "dyrektywny kompilatora"),
        ("deklaraca zmiennej", "deklaraca zmiennej"),
        ("wstawka asemblerowa", "wstawka asemblerowa"),
    )

    category = models.CharField(max_length=25,
                                choices=categories)

    def __str__(self):
        return str(self.category)


class SectionStatus(models.Model):
    status = (
        ("KOMPILUJE", "kompiluje się bez ostrzeżeń"),
        ("warning", "kompiluje się z ostrzeżeniami"),
        ("syntax error", "nie kompiluję się"),
    )
    status = models.CharField(max_length=25,
                              choices=status, null=True, default=None, blank=True)

    def __str__(self):
        if self.status is None:
            return ""
        return str(self.status)


class SectionData(models.Model):
    line_number = models.IntegerField(default=None, blank=True, null=True)
    compilatorError = models.CharField(max_length=200, null=True, default=None, blank=True, )

    def __str__(self):
        if self.line_number is None:
            return ""
        return str(self.line_number) + " " + str(self.compilatorError)


class FileSection(models.Model):
    name = models.CharField(max_length=200, default=None, blank=True, null=True)
    description = models.CharField(max_length=200, default=None, blank=True, null=True)
    cDate = models.DateTimeField("creation date", default=timezone.now())
    begin = models.IntegerField()
    end = models.IntegerField()
    sectionType = models.ForeignKey(SectionType, on_delete=models.CASCADE)
    sectionStatus = models.ForeignKey(SectionStatus, on_delete=models.CASCADE, default=section_status_getID)
    sectionData = models.OneToOneField(SectionData, on_delete=models.CASCADE, default=section_data_getID)
    file = models.ForeignKey(File, on_delete=models.CASCADE)

    def __str__(self):
        return f"{str(self.sectionType)} {str(self.begin)} {str(self.end)} {str(self.sectionStatus)} {str(self.sectionData)}"
