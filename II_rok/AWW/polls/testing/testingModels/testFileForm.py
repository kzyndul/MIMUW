from django.contrib.auth.models import User
from django.core.files.storage import default_storage
from django.core.files.uploadedfile import SimpleUploadedFile
from django.test import TestCase
from django.utils import timezone

from polls.models import Directory, FileForm


class FileFormTest(TestCase):
    def setUp(self):
        self.time = timezone.now()
        self.user = User.objects.create_user(username='testuser', password='testpass')
        self.directory = Directory.objects.create(
            name='Test Directory',
            description='This is a test directory',
            visibility=True,
            cDate=self.time,
            vDate=self.time,
            change_date=self.time,
            owner=self.user,
        )
        self.file = None

    def test_valid_form(self):
        file_data = {
            'name': 'Test File',
            'description': 'Test description',
            'filePath': SimpleUploadedFile("file.txt", b"file_content"),
            'directory': self.directory.pk,
            'cDate': timezone.now(),
            'visibility': True,
            'vDate': timezone.now(),
            'change_date': timezone.now(),
            'owner': self.user.pk
        }
        form = FileForm(data=file_data, files={'filePath': file_data['filePath']})


        self.assertTrue(form.is_valid())
        self.file = form.save()
        self.assertEqual(self.file.name, 'Test File')
        self.assertEqual(self.file.description, 'Test description')
        self.assertEqual(self.file.directory, self.directory)
        self.assertEqual(self.file.owner, self.user)
        self.assertEqual(self.file.visibility, True)

    def test_invalid_form(self):
        file_data = {'name': 'Test File',
                     'description': 'Test description',
                     'filePath': SimpleUploadedFile("file.txt", b"file_content"),
                     'directory': None,  # Invalid directory ID
                     'cDate': timezone.now(),
                     'visibility': True,
                     'vDate': timezone.now(),
                     'change_date': timezone.now(),
                     'owner': self.user.id}

        form = FileForm(data=file_data, files={'filePath': file_data['filePath']})
        self.assertFalse(form.is_valid())
        self.assertIn('directory', form.errors)

    def tearDown(self):
        if self.file and self.file.filePath:
            default_storage.delete(self.file.filePath.name)
        if self.file:
            self.file.delete()
        if self.directory:
            self.directory.delete()

        super().tearDown()
