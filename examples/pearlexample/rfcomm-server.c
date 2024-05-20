#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <bluetooth/bluetooth.h>
#include <bluetooth/rfcomm.h>

#define ABS(x) (x < 0 ? -x : x)

int main(int argc, char **argv)
{
    struct sockaddr_rc loc_addr = { 0 }, rem_addr = { 0 }; int status;
    char buf[1024] = { 0 };
    int s, client, bytes_read;
    socklen_t opt = sizeof(rem_addr);
    int random;

    // I want to use the built in BT adapter
    const char* adapter = "60:F2:62:1B:9C:74";

    srand(time(NULL));

    printf("size if: %ld\n", sizeof (loc_addr));

    // allocate socket
    s = socket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);
    if(s == -1) {
        printf("error encountered while creating the socket\n");
    } else {
        printf("successfully created the socket\n");
    }

    // bind socket to port 1 of the first available 
    // local bluetooth adapter
    loc_addr.rc_family = AF_BLUETOOTH;
    str2ba(adapter, &loc_addr.rc_bdaddr);
    loc_addr.rc_channel = (uint8_t) 1;
    int err = bind(s, (struct sockaddr *)&loc_addr, sizeof(loc_addr));
    if(err) {
        printf("error binding the socket to the addressing structure\n");
    } else {
        printf("successfully bound the socket\n");
    }

    // put socket into listening mode
    err = listen(s, 1);
    if(err == -1) {
        printf("error while setting the socket in listening mode\n");
    } else {
        printf("successfully set the socket in listening mode\n");
    }

    // accept one connection
    client = accept(s, (struct sockaddr *)&rem_addr, &opt);

    ba2str( &rem_addr.rc_bdaddr, buf );
    fprintf(stderr, "accepted connection from %s\n", buf);
    memset(buf, 0, sizeof(buf));

    random = rand();
    random = ABS(random);
    random = random % 255;
    printf("generated secret is: %d\n", random);

    bytes_read = read(client, buf, sizeof(buf));
    if(bytes_read > 0) {
        printf("received [%s] : length %lu\n", buf, strlen(buf));
    }
    if(random == (int)(((uint32_t *)buf)[0])) {
        status = write(client, "ok!", 4);
    } else {
        status = write(client, "not ok!", 8);
    }

    // close connection
    close(client);
    close(s);
    return 0;
}
