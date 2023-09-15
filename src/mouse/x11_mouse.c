#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>

#include <x11_mouse.h>

int init_mouse(){
    const char *pDevice = "/dev/input/mice";

    // Open Mouse
    int fd = open(pDevice, O_RDWR);
    if (fd == -1)
    {
        printf("ERROR Opening %s\n", pDevice);
        return -1;
    }
    return fd;
}

void get_mouse_data(int fd, Display *display, int* x, int* y, char* left, char* middle, char* right)
{
    Window root_window = DefaultRootWindow(display);
    XEvent event;
    unsigned char data[3];
    int bytes;

    bytes = read(fd, data, sizeof(data));

    if (bytes > 0)
    {
        *left = data[0] & 0x1;
        *right = data[0] & 0x2;
        *middle = data[0] & 0x4;
    }

    XQueryPointer(
        display,
        root_window,
        &event.xbutton.root,
        &event.xbutton.subwindow,
        &event.xbutton.x_root,
        &event.xbutton.y_root,
        &event.xbutton.x,
        &event.xbutton.y,
        &event.xbutton.state);

    *x = event.xmotion.x;
    *y = event.xmotion.y;

}

/*int main(int argc, char **argv)
{
    Display *display;

    int fd = init_mouse(&display);

    while (1)
    {
        int x, y, left, middle, right;
        get_mouse_data(fd, display, &x, &y, &left, &middle, &right);
        printf("x=%d, y=%d, left=%d, middle=%d, right=%d\n", x, y, left, middle, right);
    }
}*/