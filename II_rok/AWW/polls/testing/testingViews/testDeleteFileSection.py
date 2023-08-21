from django.core.files.storage import default_storage
from django.core.files.uploadedfile import SimpleUploadedFile
from django.test import TestCase
from django.contrib.auth.models import User
from django.urls import reverse

from polls.models import File, Directory, SectionType, SectionStatus, SectionData, FileSection
from django.utils import timezone

class DeleteSectionTestCase(TestCase):
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
        self.section_type = SectionType.objects.create(category='procedura')
        self.section_status = SectionStatus.objects.create(status='KOMPILUJE')
        self.section_data = SectionData.objects.create(line_number=1, compilatorError='')
        self.file_section = FileSection.objects.create(
            file=self.file,
            sectionType=self.section_type,
            sectionStatus=self.section_status,
            sectionData=self.section_data,
            begin=1,
            end=2,
            cDate=self.time
        )

    def test_delete_section(self):
        self.client.login(username='testuser', password='testpass')
        response = self.client.post(reverse('deleteSection', args=[self.file.id, self.file_section.id]))
        self.assertEqual(response.status_code, 200)
        self.assertFalse(FileSection.objects.filter(id=self.file_section.id).exists())

    def tearDown(self):
        default_storage.delete(self.file.filePath.name)
        self.file.delete()
        self.directory.delete()

        super().tearDown()
