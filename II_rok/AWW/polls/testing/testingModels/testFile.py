from django.contrib.auth.models import User
from django.core.files.storage import default_storage
from django.test import TestCase
from django.core.files.uploadedfile import SimpleUploadedFile

from polls.models import Directory, File
from django.utils import timezone


class FileModelTest(TestCase):
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

    def test_file_creation(self):
        self.assertEqual(self.file.name, 'test_file.txt')
        self.assertEqual(self.file.description, 'Test file description')
        self.assertEqual(self.file.visibility, True)
        self.assertEqual(self.file.directory, self.directory)
        self.assertEqual(self.file.owner, self.user)
        self.assertIsNotNone(self.file.filePath)

    def tearDown(self):
        default_storage.delete(self.file.filePath.name)
        self.file.delete()
        self.directory.delete()

        super().tearDown()