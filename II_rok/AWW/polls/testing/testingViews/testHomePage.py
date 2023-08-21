from django.core.files.storage import default_storage
from django.core.files.uploadedfile import SimpleUploadedFile
from django.test import TestCase
from django.contrib.auth.models import User
from polls.models import Directory, File, FileForm, DirectoryForm
from polls.views import get_context
from django.utils import timezone

class HomePageTest(TestCase):
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
        self.client.login(username='testuser', password='testpass')
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
    def test_get_context(self):
        request = self.client.get('/')
        request.user = self.user

        context = get_context(request)

        self.assertIsInstance(context['fileForm'], FileForm)
        self.assertEqual(context['fileForm'].fields['owner'].initial, self.user)
        self.assertIsInstance(context['directoryForm'], DirectoryForm)
        self.assertEqual(context['directoryForm'].fields['owner'].initial, self.user)
        self.assertEqual(context['db_files'], [[self.directory, [self.file]]])

    def tearDown(self):
        default_storage.delete(self.file.filePath.name)
        self.file.delete()
        self.directory.delete()

        super().tearDown()
