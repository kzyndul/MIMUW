from django.contrib.auth.models import User
from django.core.files.storage import default_storage
from django.core.files.uploadedfile import SimpleUploadedFile
from django.test import TestCase
from django.utils import timezone

from polls.models import File, DeleteFileForm, Directory


class DeleteFileFormTest(TestCase):

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
        self.file = File.objects.create(
            name='test_file.txt',
            description='Test file description',
            visibility=True,
            cDate=self.time,
            vDate=self.time,
            change_date=self.time,
            directory=self.directory,
            filePath=SimpleUploadedFile('test_file.txt', b'Test content'),
            owner=self.user,
        )

    def test_form_valid(self):
        data = {'id': self.file.pk}
        form = DeleteFileForm(data=data)
        self.assertTrue(form.is_valid())

    def test_form_invalid(self):
        data = {'id': 'invalid_id'}
        form = DeleteFileForm(data=data)
        self.assertFalse(form.is_valid())


    def tearDown(self):
        default_storage.delete(self.file.filePath.name)
        self.file.delete()
        self.directory.delete()

        super().tearDown()
