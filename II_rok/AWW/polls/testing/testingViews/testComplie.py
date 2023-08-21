from django.core.files.storage import default_storage
from django.core.files.uploadedfile import SimpleUploadedFile
from django.test import TestCase
from django.contrib.auth.models import User
from django.urls import reverse
from django.utils import timezone

from polls.models import Directory, File, SectionType, SectionStatus, SectionData, FileSection


class CompileTest(TestCase):
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
            filePath=SimpleUploadedFile('test_file.txt', b'int main(void) {return 0;}'),
            owner=self.user,
        )

        self.section_type = SectionType.objects.create(category='procedura')
        self.section_status = SectionStatus.objects.create(status='KOMPILUJE')
        self.section_data = SectionData.objects.create(line_number=1, compilatorError='')

        self.file_section = FileSection.objects.create(
            name='test_section',
            description='test_description',
            cDate=self.time,
            begin=1,
            end=10,
            sectionType=self.section_type,
            sectionStatus=self.section_status,
            sectionData=self.section_data,
            file=self.file,
        )

    def test_compile_file_redirect(self):
        self.client.login(username='testuser', password='testpass')
        response = self.client.post(reverse('compile', kwargs={'pk': self.file.id}))
        self.assertEqual(response.status_code, 200)
        file_section = FileSection.objects.get(id=self.file_section.id)
        self.assertEqual(file_section.sectionStatus.status, 'KOMPILUJE')

    def tearDown(self):
        default_storage.delete(self.file.filePath.name)
        self.file.delete()
        self.directory.delete()

        super().tearDown()
