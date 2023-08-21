from django.core.files.storage import default_storage
from django.test import TestCase, RequestFactory
from django.contrib.auth.models import User
from django.core.files.uploadedfile import SimpleUploadedFile
from django.urls import reverse

from polls.models import Directory, File, DirectoryForm, FileForm
from polls.views import homePage, get_context_file
from django.utils import timezone


class HomePageWithFileTestCase(TestCase):
    def setUp(self):
        self.factory = RequestFactory()
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
            filePath=SimpleUploadedFile('test_file.txt', b'int main(void) {return 0;}'),
            owner=self.user,
        )


        another_user = User.objects.create_user(username='anotheruser', password='testpass')
        self.anotherFile = File.objects.create(
            name='another_file.txt',
            description='Another file description',
            visibility=True,
            cDate=self.time,
            vDate=self.time,
            change_date=self.time,
            directory=self.directory,
            filePath=SimpleUploadedFile('another_file.txt', b'int main(void) {return 0;}'),
            owner=another_user,
        )

    def test_homePage_get(self):
        request = self.factory.get('/home/')
        request.user = self.user
        response = homePage(request)
        self.assertEqual(response.status_code, 200)

    def test_file_and_directory_forms_in_context(self):
        response = self.client.get(reverse('homePage'))
        self.assertIsInstance(response.context['fileForm'], FileForm)
        self.assertIsInstance(response.context['directoryForm'], DirectoryForm)

    def test_get_context_file(self):
        request = self.factory.get('/home/')
        request.user = self.user
        context = get_context_file(request, self.file)
        self.assertEqual(context['file'][0], 'int main(void) {return 0;}')
        self.assertEqual(len(context['sectionList']), 0)
        self.assertEqual(context['file_index'], self.file.id)


    # def test_getHomePageFile_post_valid_form(self):  #TODO napraaw
    #     self.client.login(username='testuser', password='testpassword')
    #     file_data = {
    #         'name': 'Test File',
    #         'description': 'Test description',
    #         'filePath': SimpleUploadedFile("file.txt", b"file_content"),
    #         'directory': self.directory.pk,
    #         'cDate': self.time,Next
    #         'visibility': True,
    #         'vDate': self.time,
    #         'change_date': self.time,
    #         'owner': self.user.pk
    #     }
    #     response = self.client.post(reverse('getHomePageFile', kwargs={'pk': 1}), data=file_data, files={'filePath': file_data['filePath']})
    #
    #     self.assertEqual(response.status_code, 200)
    #
    #     self.assertEqual(File.objects.count(), 1)

    def test_db_files_in_context_are_owned_by_logged_in_user(self):
        response = self.client.get(reverse('homePage'))
        db_files = response.context['db_files']

        self.assertEqual(len(db_files), 1)
        self.assertEqual(db_files[0][0], self.directory)
        self.assertEqual(db_files[0][1][0], self.file)

    def tearDown(self):
        if self.file and self.file.filePath:
            default_storage.delete(self.file.filePath.name)
        if self.file:
            self.file.delete()
        if self.directory:
            self.directory.delete()

        if self.anotherFile and self.anotherFile.filePath:
            default_storage.delete(self.anotherFile.filePath.name)
        if self.anotherFile:
            self.anotherFile.delete()

        another_user = User.objects.filter(username='anotheruser').first()
        if another_user:
            another_user.delete()

        self.user.delete()

        super().tearDown()
