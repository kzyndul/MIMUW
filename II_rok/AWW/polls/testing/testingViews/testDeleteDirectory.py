from django.test import TestCase
from django.urls import reverse
from django.contrib.auth.models import User
from polls.models import Directory
from django.utils import timezone
class DeleteDirectoryViewTest(TestCase):

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

    def test_directory_visibility_changes_on_success(self):
        self.client.login(username='testuser', password='testpass')
        url = reverse('deleteDirectory', args=[self.directory.pk])
        self.client.post(url)
        self.directory.refresh_from_db()
        self.assertFalse(self.directory.visibility)

    def test_view_returns_404_if_directory_not_found(self):
        self.client.login(username='testuser', password='testpass')
        url = reverse('deleteDirectory', args=[999])
        response = self.client.post(url)
        self.assertEqual(response.status_code, 404)