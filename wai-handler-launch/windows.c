#include <windows.h>
#include <shellapi.h>
#include <stdio.h>

void launch(int port, char *s)
{
    int len = 8 + strlen("http://127.0.0.1:") + strlen(s);
    char *buff = malloc(len);
    snprintf(buff, len, "http://127.0.0.1:%d/%s", port, s);
    ShellExecute(NULL, "open", buff, NULL, NULL, SW_SHOWNORMAL);
    free(buff);
}
