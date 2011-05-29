#include <windows.h>
#include <shellapi.h>

void launch(void)
{
    ShellExecute(NULL, "open", "http://127.0.0.1:4587/", NULL, NULL, SW_SHOWNORMAL);
}
