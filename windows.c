#include <windows.h>
#include <shellapi.h>

void launch(void)
{
    ShellExecute(NULL, "open", "http://localhost:4587/", NULL, NULL, SW_SHOWNORMAL);
}
