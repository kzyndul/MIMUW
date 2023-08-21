from django.contrib.auth.models import User
from django.test import TestCase
from django.utils import timezone

from polls.models import DirectoryForm


class DirectoryFormTestCase(TestCase):
    def test_valid_form(self):
        user = User.objects.create(username='testuser')
        user.save()

        data = {
            'name': 'Test Directory',
            'description': 'This is a test directory',
            'cDate': timezone.now(),
            'visibility': True,
            'vDate': timezone.now(),
            'change_date': timezone.now(),
            'owner': user.pk,
        }
        form = DirectoryForm(data=data)
        self.assertTrue(form.is_valid())

    def test_invalid_form(self):
        data = {
            'name': '',
            'description': 'This is a test directory',
            'cDate': timezone.now(),
            'visibility': True,
            'vDate': timezone.now(),
            'change_date': timezone.now(),
            'owner': '',
        }
        form = DirectoryForm(data=data)
        self.assertFalse(form.is_valid())