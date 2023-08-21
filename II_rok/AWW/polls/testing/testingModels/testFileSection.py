from django.contrib.auth.models import User
from django.core.files.storage import default_storage
from django.core.files.uploadedfile import SimpleUploadedFile
from django.db import IntegrityError, transaction
from django.test import TestCase

from polls.models import File, SectionType, SectionStatus, SectionData, FileSection, Directory

from django.utils import timezone


class FileSectionModelTest(TestCase):
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
        self.section_type = SectionType.objects.create(category='procedura')
        self.section_status = SectionStatus.objects.create(status='KOMPILUJE')
        self.section_data = SectionData.objects.create(line_number=1, compilatorError='')
        self.section_data1 = SectionData.objects.create(line_number=1, compilatorError='')

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

    def test_file_section_creation(self):

        self.assertEqual(self.file_section.name, 'test_section')
        self.assertEqual(self.file_section.description, 'test_description')
        self.assertEqual(str(self.file_section.sectionType), 'procedura')
        self.assertEqual(str(self.file_section.sectionStatus), 'KOMPILUJE')
        self.assertEqual(str(self.file_section.sectionData), '1 ')
        self.assertEqual(self.file_section.file, self.file)

    def test_string_representation(self):

        self.assertEqual(str(self.file_section), 'procedura 1 10 KOMPILUJE 1 ')

    def test_section_status_is_optional(self):
        file_section = FileSection.objects.create(
            name='Test File Section',
            description='This is a test file section',
            begin=1,
            end=10,
            sectionType=self.section_type,
            sectionData=self.section_data1,
            file=self.file,
        )
    def test_section_data_is_optional(self):
        file_section = FileSection.objects.create(
            name='Test File Section',
            description='This is a test file section',
            begin=1,
            end=10,
            sectionType=self.section_type,
            sectionStatus=self.section_status,
            file=self.file,
        )
    def test_file_is_required(self):
        with self.assertRaises(IntegrityError):
            file_section = FileSection.objects.create(
                name='Test File Section',
                description='This is a test file section',
                begin=1,
                end=10,
                sectionType=self.section_type,
                sectionStatus=self.section_status,
                sectionData=self.section_data,
            )
            file_section.full_clean()

    def test_section_type_is_required(self):
        with self.assertRaises(IntegrityError):
            file_section = FileSection.objects.create(
                name='Test File Section',
                description='This is a test file section',
                begin=1,
                end=10,
                sectionStatus=self.section_status,
                sectionData=self.section_data,
                file=self.file,
            )
            file_section.full_clean()

    def tearDown(self):
        try:
            with transaction.atomic():
                default_storage.delete(self.file.filePath.name)
                self.file.delete()
                self.directory.delete()
        except transaction.TransactionManagementError:
            pass
        super().tearDown()