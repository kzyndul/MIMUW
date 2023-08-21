from django.core.files.storage import default_storage
from django.core.files.uploadedfile import SimpleUploadedFile
from django.test import TestCase
from django.contrib.auth.models import User
from django.utils import timezone

from polls.models import File, FileSection, Directory


class CreateSectionTestCase(TestCase):

    def setUp(self):
        self.user = User.objects.create_user(username='testuser', password='testpass')
        self.time = timezone.now()
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

    def test_create_section(self):
        self.client.login(username='testuser', password='testpass')

        response = self.client.post('/createSection/{}/'.format(self.file.id), {
            'selectedTextFileSection': 'Test content',
            'sectionTypeForm': 'procedura',
            'selectFileName': 'Test section name',
            'selectFileDescription': 'Test section description',
        })
        self.assertTrue(response.status_code, 200)

        self.assertEqual(FileSection.objects.count(), 1)
        section = FileSection.objects.first()
        self.assertEqual(section.name, 'Test section name')
        self.assertEqual(section.description, 'Test section description')
        self.assertEqual(section.begin, 1)
        self.assertEqual(section.end, 1)
        self.assertEqual(section.file, self.file)

    def tearDown(self):
        default_storage.delete(self.file.filePath.name)
        self.file.delete()
        self.directory.delete()

        super().tearDown()
