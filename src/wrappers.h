#include <X11/Xlib.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <X11/extensions/XShm.h>

#include <sys/time.h>
#include <stdio.h>

XShmSegmentInfo shminfo;
static int ErrorFlag = 0;

static int HandleXError(Display *dpy, XErrorEvent *event)
{
    ErrorFlag = 1;
    return 0;
}

void wrap_x_shm_put_image (Display* dpy ,
                           Drawable	d,
                           GC gc,
                           XImage* image,
                           unsigned int	src_width,
                           unsigned int	src_height) {
    XShmPutImage(dpy, d, gc, image, 0, 0, 0, 0, src_width, src_height, False);
}

XImage* wrap_alloc_xshm_image(Display *dpy, 
                              Visual *vis,
                              int width, 
                              int height, 
                              int depth)
{
    XImage *img;

    img = XShmCreateImage(dpy, vis, depth,
                          ZPixmap, NULL, &shminfo,
                          width, height);

    shminfo.shmid = shmget(IPC_PRIVATE, img->bytes_per_line * img->height, IPC_CREAT | 0777);

    shminfo.shmaddr = img->data = (char *)shmat(shminfo.shmid, 0, 0);

    shminfo.readOnly = False;
    ErrorFlag = 0;
    XSetErrorHandler(HandleXError);

    /* This may trigger the X protocol error we're ready to catch: */
    XShmAttach(dpy, &shminfo);
    XSync(dpy, False);

    shmctl(shminfo.shmid, IPC_RMID, 0); /* nobody else needs it */

    return img;
}