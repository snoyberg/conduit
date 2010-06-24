#include <QApplication>
#include <QPushButton>
#include <QtWebKit>

extern "C" {
    int startBrowser()
    {
        char title[80] = "My Application";
        char *argv[1];
        argv[0] = title;
        int argc = 1;

        QApplication app(argc, argv);
        QWebView *view = new QWebView();
        view->load(QUrl("http://localhost:3000/"));
        view->show();

        return app.exec();
    }
}
