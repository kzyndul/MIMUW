from django.test import TestCase, Client
from django.urls import reverse
from django.contrib.auth.models import User



class HomePageDirectoryViewTestCase(TestCase):
    def setUp(self):
        self.client = Client()
        self.user = User.objects.create_user(username='testuser', password='12345')
        self.client.login(username='testuser', password='12345')
        self.url = reverse('homePage')

    def test_get(self):
        response = self.client.get(self.url)
        self.assertEqual(response.status_code, 200)
        self.assertTemplateUsed(response, 'polls/homePage.html')
        self.assertContains(response, 'directoryForm')

    # def test_post_success(self): # TODO napraw
    #     data = {
    #         'name': 'Test Directory',
    #         'description': 'This is a test directory',
    #         'cDate': timezone.now(),
    #         'visibility': True,
    #         'vDate': timezone.now(),
    #         'change_date': timezone.now(),
    #         'owner': self.user.pk,
    #     }
    #     response = self.client.post(self.url, data)
    #     self.assertEqual(response.status_code, 200)
    #     response = response = self.client.post('/polls/getHomePageDirectory/', data=data)
    #     # Check if the directory object has been created
    #     self.assertTrue(Directory.objects.filter(id=1).exists())
    #
    #     directory = Directory.objects.get(name='Test Directory')
    #     self.assertEqual(directory.owner, self.user)
