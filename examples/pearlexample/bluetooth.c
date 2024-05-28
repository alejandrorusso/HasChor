#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <stdint.h>
#include <string.h>

#include <dlfcn.h>

// bluetooth .so
char *btso = "/usr/lib/x86_64-linux-gnu/libbluetooth.so";

/* known remote bluetooth adapter */
const char dest[18] = "60:F2:62:1B:9C:74";

int allocate_socket_() {
    int s; int f; void *handle; char *error;

    handle = dlopen(btso, RTLD_LAZY);
    if(!handle) {
        printf("%s\n", dlerror());
        exit(1);
    }

    s = socket(AF_BLUETOOTH, SOCK_STREAM, 3); // I hardcode the 3 here, as it is defined as a macro
                                              // I don't think I can dlsym it
    if(s == -1) {
        printf("error encountered while creating the socket\n");
    }
    return s;
}

void connect_(int socket) {
    uint8_t addr[10]; void *handle; char *error; int status;
    int (*str2ba)(const char *, uint8_t *);

    addr[0] = AF_BLUETOOTH;
    addr[8] = (uint8_t) 1;

    handle = dlopen(btso, RTLD_LAZY);
    if(!handle) {
        printf("%s\n", dlerror());
        exit(1);
    }
    dlerror();

    str2ba = (int (*)(const char *, uint8_t *)) dlsym(handle, "str2ba");
    error = dlerror();
    if(error != NULL) {
        printf("%s\n", error);
        exit(1);
    }
    str2ba(dest, &addr[2]);

    status = connect(socket, (struct sockaddr *)addr, 10);

    // send a message
    if( status == 0 ) {
        printf("successfully connected\n");
    } else {
        perror("error connecting");
    }

    return;

}

uint64_t authenticate_(int socket, uint32_t secret) {
    int status; int bytes_read; char msg[12]; char resp[40]; uint8_t token[8];
    printf("given secret is %d\n", secret);

    memset(msg, 0, 12);
    memcpy(msg, (uint8_t *)&secret, sizeof(secret));
    status = write(socket, msg, strlen(msg));

    if(status < 0) {
        printf("error while writing the message\n");
        exit(1);
    }

    memset(resp, 0, 40);
    bytes_read = read(socket, resp, sizeof(resp));
    if(bytes_read <= 0) {
        printf("received just %d bytes\n", bytes_read);
    }

    printf("received %d bytes\n", bytes_read);
    printf("received [%s]\n", resp);

    if(bytes_read == 8) {
        for (int i = 0; i < 8; i++) {
            token[i] = (uint8_t) resp[i];
        }
        return ((uint64_t *)resp)[0];
    } else if (bytes_read == 1) {
        return (uint64_t) 0;
    } else {
        printf("erroneous result code returned from server [%s]\n", resp);
        exit(1);
    }

}

void close_(int socket) {
    int err;
    err = close(socket);
    if(err) {
        printf("error closing socket: %d\n", err);
    }
}