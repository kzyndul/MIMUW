from django.test import TestCase
from django.contrib.auth.models import User

from polls.models import Directory

from django.utils import timezone

time = timezone.now()
class DirectoryModelTest(TestCase):
    def setUp(self):
        self.user = User.objects.create_user(username='testuser', password='testpass')
        self.directory = Directory.objects.create(
            name='Test Directory',
            description='This is a test directory',
            visibility=True,
            cDate=time,
            vDate=time,
            change_date=time,
            owner=self.user,
        )

    def test_directory_creation(self):
        self.assertEqual(self.directory.name, 'Test Directory')
        self.assertEqual(self.directory.description, 'This is a test directory')
        self.assertTrue(self.directory.visibility)
        self.assertEqual(self.directory.cDate, time)
        self.assertEqual(self.directory.vDate, time)
        self.assertEqual(self.directory.change_date, time)
        self.assertEqual(self.directory.owner, self.user)

    def test_directory_str(self):
        self.assertEqual(str(self.directory), 'Test Directory This is a test directory')
