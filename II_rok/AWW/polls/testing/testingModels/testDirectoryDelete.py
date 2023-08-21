from django.contrib.auth.models import User
from django.test import TestCase
from polls.models import Directory, DeleteDirectoryForm

from django.utils import timezone


class DeleteDirectoryFormTest(TestCase):


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
    def test_form_valid(self):
        data = {'id': self.directory.pk}
        form = DeleteDirectoryForm(data=data)
        self.assertTrue(form.is_valid())
    def test_form_invalid(self):
        data = {'id': 'invalid_id'}
        form = DeleteDirectoryForm(data=data)
        self.assertFalse(form.is_valid())
