from django.contrib.auth.models import User
from django.contrib.auth import authenticate, login
from django.test import TestCase, Client
from django.urls import reverse

from polls.models import LoginForm


class LoginTestCase(TestCase):
    def setUp(self):
        self.client = Client()
        self.user = User.objects.create_user(
            username='testuser', password='testpass'
        )
        self.url = reverse('login')

    def test_login_form_valid(self):
        data = {'username': 'testuser', 'password': 'testpass'}
        response = self.client.post(self.url, data)
        self.assertEqual(response.status_code, 302)  # Redirect to home page
        user = authenticate(username='testuser', password='testpass')
        self.assertIsNotNone(user)
        self.assertEqual(user, self.user)

    def test_login_form_invalid(self):
        data = {'username': 'testuser', 'password': 'wrongpass'}
        response = self.client.post(self.url, data)
        user = authenticate(username='testuser', password='wrongpass')
        self.assertIsNone(user)


    def test_login_form_displayed(self):
        response = self.client.get(self.url)
        self.assertEqual(response.status_code, 200)
        self.assertTemplateUsed(response, 'polls/loginPage.html')
        self.assertIsInstance(response.context['form'], LoginForm)