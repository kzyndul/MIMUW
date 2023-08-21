from django.urls import path

from .views import homePage, compileFile, \
    deleteSection, deleteFile, deleteDirectory, loginPage, getHomePageFile, getHomePageDirectory, createSection

urlpatterns = [
    path('login/', loginPage, name='login'),
    path('', homePage, name='homePage'),

    path('deleteDirectory/<int:pk>/', deleteDirectory, name='deleteDirectory'),
    path('deleteFile/<int:pk>/', deleteFile, name='deleteFile'),

    path('deleteSection/<int:pk>/<int:flieSectionPK>/', deleteSection, name='deleteSection'),
    path('createSection/<int:pk>/', createSection, name='createSection'),

    path('compile/<int:pk>/', compileFile, name='compile'),


    path('getHomePageFile/<int:pk>/', getHomePageFile, name ='getHomePageFile'),
    path('getHomePageDirectory/', getHomePageDirectory, name ='getHomePageDirectory')
]