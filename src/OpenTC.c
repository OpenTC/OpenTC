/*
Copyright (c) 2017 João Pedro Finoto Martins

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

#include "OpenTC.h"
#include <stdio.h>
#include <string.h>

#define UINT8VAL(u) (*(u))
#define UINT16VAL(u) ((((uint16_t) *(u)) << 8) + ((uint16_t) *((u) + 1)))
#define UINT32VAL(u) ((((uint32_t) *(u)) << 24) + (((uint32_t)*((u) + 1)) << 16) + (((uint32_t)*((u) + 2)) << 8) + ((uint32_t)*((u) + 3)))
#define UINT64VAL(u) ((((uint64_t) *(u)) << 56) + (((uint64_t) *((u) + 1)) << 48) + (((uint64_t) *((u)) + 3) << 32) + (((uint64_t) *((u)) + 4) << 24) + (((uint64_t) *((u)) + 5) << 16) + (((uint64_t) *((u)) + 6) << 8) + ((uint64_t) *((u) + 7)))

#define VALUINT8(u, val) (*(u) = (val))
#define VALUINT16(u, val) (*(u) = ((val) >> 8) & 0xff); (*((u) + 1) = (val) & 0xff)
#define VALUINT32(u, val) (*(u) = ((val) >> 24) & 0xff); (*((u) + 1) = ((val) >> 16) & 0xff); (*((u) + 2) = ((val) >> 8) & 0xff); (*((u) + 3) = ((val)) & 0xff)
#define VALUINT64(u, val) (*(u) = ((val) >> 56) & 0xff); (*((u) + 1) = ((val) >> 48) & 0xff); (*((u) + 2) = ((val) >> 32) & 0xff); (*((u) + 3) = ((val) >> 24) & 0xff); (*((u) + 4) = ((val) >> 16) & 0xff); (*((u) + 6) = ((val) >> 8) & 0xff); (*((u) + 7) = ((val)) & 0xff)

#define DEBUG(...)
#define NDEBUG(...)
#define DEBUGMESSAGE(...)

//#define DEBUG(...) printf(__VA_ARGS__); printf("\n")
//#define NDEBUG(...) printf("%s: ", node->name); DEBUG(__VA_ARGS__)

//void DEBUGMESSAGE(const uint8_t* message, TcLength_t len) {
//    while(len--) {
//        printf("%02x", *(message++));
//    }
//
//    putchar('\n');
//}

void tcInit() {

}

TcStatus_t tcInitNode(TcNode_t* node) {
    int i;
    node->vars[0] = NULL;
    node->services[0] = NULL;

    if(strlen(node->name) > 0xff) {
        return TC_FIELD_TOO_BIG;
    }


    // zero-fill list types

    for (i = 0; i < node->connectionsMax; i++) {
        node->connections[i] = NULL;
    }

    for (i = 0; i < node->varsMax; i++) {
        node->vars[i] = NULL;
    }

    for (i = 0; i < node->servicesMax; i++) {
        node->services[i] = NULL;
    }

    for (i = 0; i < node->varCallbacksMax; i++) {
        node->varCallbacks[i] = NULL;
    }

    for (i = 0; i < node->serviceStoreMax; i++) {
        node->serviceStore[i].ref = 0xffff;
    }

    for (i = 0; i < node->serialsMax; i++) {
        node->serials[i] = NULL;
    }

    if (node->root) {
        for (i = 0; i < node->varRequestsMax; i++) {
            node->varRequests[i].node = 0;
        }
    }


    return TC_SUCCESS;
}

void tcSetArray8(TcVarstore_t* var, uint8_t at, uint8_t val) {
    tcSetVec8(var->array.storage + 2, at, val);
}

void tcSetArray16(TcVarstore_t* var, uint8_t at, uint16_t val) {
    tcSetVec16(var->array.storage + 2, at, val);
}

void tcSetArray32(TcVarstore_t* var, uint8_t at, uint32_t val) {
    tcGetVec32(var->array.storage + 2, at);
}

void tcSetArray64(TcVarstore_t* var, uint8_t at, uint64_t val) {
    tcSetVec64(var->array.storage + 2, at, val);
}

uint8_t tcGetArray8(TcVarstore_t* var, uint8_t at) {
    return tcGetVec8(var->array.storage + 2, at);
}

uint16_t tcGetArray16(TcVarstore_t* var, uint8_t at) {
    return tcGetVec16(var->array.storage + 2, at);
}

uint32_t tcGetArray32(TcVarstore_t* var, uint8_t at) {
    return tcGetVec32(var->array.storage + 2, at);
}

uint64_t tcGetArray64(TcVarstore_t* var, uint8_t at) {
    return tcGetVec64(var->array.storage + 2, at);
}

void tcArraySetLength(TcVarstore_t* var, uint16_t length) {
    var->array.length = length;
    VALUINT16(var->array.storage, length);
}

void tcSetVec8(uint8_t* buffer, uint8_t at, uint8_t val) {
    VALUINT8(buffer + at, val);
}

void tcSetVec16(uint8_t* buffer, uint8_t at, uint16_t val) {
    VALUINT16(buffer + 2 * at, val);
}

void tcSetVec32(uint8_t* buffer, uint8_t at, uint32_t val) {
    VALUINT32(buffer + 4 * at, val);
}

void tcSetVec64(uint8_t* buffer, uint8_t at, uint64_t val) {
    VALUINT64(buffer + 8 * at, val);
}

uint8_t tcGetVec8(const uint8_t* buffer, uint8_t at) {
    return UINT8VAL(buffer + at);
}

uint16_t tcGetVec16(const uint8_t* buffer, uint8_t at) {
    return UINT16VAL(buffer + 2 * at);
}

uint32_t tcGetVec32(const uint8_t* buffer, uint8_t at) {
    return UINT32VAL(buffer + 4 * at);
}

uint64_t tcGetVec64(const uint8_t* buffer, uint8_t at) {
    return UINT16VAL(buffer + 8 * at);
}

TcLength_t tcAssembleMessageNodeCon(TcNode_t* node, TcConnection_t* connection) ;

TcStatus_t tcInitConnection(TcNode_t* node, TcConnection_t* conn) {
    TcLength_t pos;
    TcStatus_t status = tcPush((const void**) node->connections, conn, node->connectionsMax, &pos);

    if(status != TC_SUCCESS) {
        return status;
    }

    conn->id = (uint8_t) pos;

    if(!node->root && conn->id == 0) {
        // this connection is to the parent

        TcLength_t length = tcAssembleMessageNodeCon(node, conn);
        conn->handler(node, conn, node->messageBuffer, length);
    }

    return TC_SUCCESS;
}

TcStatus_t tcPush(const void** container, const void* elem, TcLength_t maxLength, TcLength_t * index) {
    TcLength_t num = tcNumElements(container);

    if(maxLength == 0 || num >= (maxLength - 1)) {
        return TC_OVERFLOW;
    }

    container[num] = elem;
    container[num + 1] = NULL;

    if(index != NULL) {
        *index = num;
    }

    return TC_SUCCESS;
}

TcLength_t tcNumElements(const void** elem) {
    TcLength_t len = 0;

    while(*(elem++) != NULL) {
        len++;
    }

    return len;
}

static TcMessage_t tempMessage;

TcStatus_t tcForward(TcNode_t* node, TcMessage_t* message, TcConnection_t* originalConnection, uint8_t* buffer);

TcConnection_t* tcGetConnection(TcNode_t* node, TcAddress_t destination);

void tcFillMessageHeader(TcMessage_t* message, uint8_t* buffer) {
    message->length = UINT16VAL(buffer);
    buffer += 2;
    message->flags = *buffer;
    buffer += 1;
    message->destination = UINT16VAL(buffer);
    buffer += 2;
    message->source = UINT16VAL(buffer);
    buffer += 2;
    message->data = buffer;
}

TcBool_t tcConnectionHas(TcConnection_t* connection, TcAddress_t address) {
    TcAddress_t* temp = connection->children;

    if (temp == NULL) {
        DEBUG("Connection is NULL");
        return FALSE;
    }

    while (*temp != 0) {
        if (*temp == address) {
            return TRUE;
        }
        temp++;
    }

    return FALSE;
}

/**
 * Extracts a frame from the buffer, and returns a pointer to the next frame
 * @param message
 * @param buffer
 * @param frame
 * @return pointer to [end of frame + 1]
 */
uint8_t* tcGetFrame(TcMessage_t* message, uint8_t* buffer, TcFrame_t* frame) {
    frame->length = UINT16VAL(buffer);

    if (frame->length == 0) {
        return NULL;
    }

    DEBUG("Parsing frame - length: %d", frame->length);
    DEBUGMESSAGE(buffer, frame->length);

    buffer += 2;
    frame->type = *buffer;
    buffer += 1;
    frame->data = buffer;
    buffer += frame->length - 3;

    return buffer;
}

/* Handlers */

TcBool_t tcFrameHandleNodeCon(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame);

// this frame type delivers var updates
TcBool_t tcFrameHandleVarValue(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame);

TcBool_t tcFrameHandleRequestUpdates(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame);

TcLength_t tcAssembleMessageUpdateRequest(TcNode_t* node, TcConnection_t* connection, TcAddress_t remote, TcAddress_t var) ;

TcBool_t tcFrameHandleLocalName(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame);

/**
 * Searches a node's custom handler list for a given type
 * @param node
 * @param frameType
 * @return then handler or NULL
 */
TcFrameHandler_t tcSearchCustomFrameParser(TcNode_t* node, TcFrameType_t frameType) {
    TcCustomFrameHandler_t* handler = node->customHandlers;

    while (handler) {
        if (handler->type == frameType) {
            return handler->handler;
        }

        handler++;
    }

    return NULL;
}

TcBool_t tcFrameHandleServiceCall(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) ;

TcBool_t tcFrameHandleServiceResponse(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) ;

TcBool_t tcFrameHandleSerialData(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) ;

/**
 * Handles a single frame
 * @param node
 * @param message
 * @param frame
 * @return
 */
TcBool_t tcHandleFrame(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) {
    TcFrameHandler_t parser = NULL;
    switch (frame->type) {
        case TC_FRAME_NODECON:
            // we don't forward these messages, they should be sent with destination TC_BUBBLE
            // so they already get forwarded by the message layer
            parser = &tcFrameHandleNodeCon;
            break;

        case TC_FRAME_VARVALUE:
            parser = &tcFrameHandleVarValue;
            break;

        case TC_FRAME_REQUEST_UPDATES:
            parser = &tcFrameHandleRequestUpdates;
            break;

        case TC_FRAME_LOCALNAME:
            parser = &tcFrameHandleLocalName;
            break;

        case TC_FRAME_SERVICE_CALL:
            parser = &tcFrameHandleServiceCall;
            break;

        case TC_FRAME_SERVICE_RESPONSE:
            parser = &tcFrameHandleServiceResponse;
            break;

        case TC_FRAME_SERIALDATA:
            parser = &tcFrameHandleSerialData;
            break;

        default:
            // unknown frame?
            NDEBUG("Unknown or custom frame");
            parser = tcSearchCustomFrameParser(node, frame->type);
    }

    return parser ? parser(node, connection, message, frame) : FALSE;
}

/**
 * Process a message and returns TRUE if all frames succeeded, FALSE otherwise
 * @param node
 * @param message
 * @return
 */
TcBool_t tcHandle(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message) {
    TcFrame_t frame;
    uint8_t* buff = message->data;

    while (buff - message->data < (message->length - TC_HEADER_SIZE)) {
        buff = tcGetFrame(message, buff, &frame);

        if (!buff) {
            break;
        }

        NDEBUG("Frame: %u", frame.type);
        if (!tcHandleFrame(node, connection, message, &frame)) {
            NDEBUG("Frame handle failed");
            return FALSE;
        }
    }

    return TRUE;
}

/**
 *
 * @param node
 * @param connection
 * @return weather this message was handled (forward or successful parse) or not
 */
TcBool_t tcProcessMessage(TcNode_t* node, TcConnection_t* connection) {
    tcFillMessageHeader(&tempMessage, connection->inBuffer);

    // test if the current message is complete
    if (tempMessage.length > connection->inBufferPos) {
        return FALSE;
    }

    NDEBUG("Process Message");
    DEBUGMESSAGE(connection->inBuffer, tempMessage.length);

    if (node->observer) {
        NDEBUG("Send message to observer");
        node->observer(node, &tempMessage);
    }

    // reset in buffer
    connection->inBufferPos = 0;

    // check if we should forward this message
    if (tempMessage.destination != node->addr || tempMessage.destination == TC_BUBBLE) {
        NDEBUG("Forwarding message");
        tcForward(node, &tempMessage, connection, connection->inBuffer);

        if (tempMessage.destination == TC_BUBBLE) {
            NDEBUG("Handle bubble");
            return tcHandle(node, connection, &tempMessage);
        }

        return TRUE;
    }

    NDEBUG("Handle message");

    // handle message
    return tcHandle(node, connection, &tempMessage);
}

// forwards a message to the first possible connection, falls back to the parent
// never returns a message to the original connection
TcStatus_t tcForward(TcNode_t* node, TcMessage_t* message, TcConnection_t* originalConnection, uint8_t* buffer) {
    // get a connection that has the destination
    TcConnection_t* connection = tcGetConnection(node, message->destination);

    if (connection == originalConnection && originalConnection != NULL) {
        // ignore sending via the same connection
        NDEBUG("Won't forward: same connection");
        return TC_ERROR;
    }

    if (connection != NULL) {
        NDEBUG("Route found on connection %d", connection->id);
        // we know the route! send on this connection
        connection->handler(node, connection, buffer, message->length);
        return TC_SUCCESS;
    }

    // we don't know this address
    // check if there is a root connection and that the message
    // didn't come from the root
    if (!node->root && (originalConnection == NULL || originalConnection->id != 0)) {
        NDEBUG("Send to root");
        node->connections[0]->handler(node, connection, buffer, message->length);
        return TC_SUCCESS;
    }

    NDEBUG("Won't forward: probably root");

    return TC_ERROR;
}

TcConnection_t* tcGetConnection(TcNode_t* node, TcAddress_t destination) {
    TcConnection_t** connection = node->connections;

    while(*connection) {
        if (tcConnectionHas(*connection, destination)) {
            return *connection;
        }
        connection++;
    }

    return NULL;
}

TcBool_t tcFrameHandleNodeCon(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) {
    NDEBUG("Handle NodeCon");
    TcAddress_t address = message->flags & TC_FLAG_ASSUMESOURCE ? message->source : UINT16VAL(frame->data);
    TcAddress_t* ptr = connection->children;

    NDEBUG("Connection from node %04x on conn %d", address, connection->id);

    if(tcConnectionHas(connection, address)) {
        NDEBUG("Connection already has node");
        return TRUE;
    }

    while (*ptr) {
        ptr++;
    }

    if (ptr - connection->children >= connection->childrenMax) {
        NDEBUG("Connection children overflow");
        return FALSE;
    }

    *ptr = address;
    *(ptr + 1) = 0;

    NDEBUG("Added to children");

    return TRUE;
}

typedef enum {
    TC_PRIMITIVE_UINT8 = 0xF11,
    TC_PRIMITIVE_UINT16 = 0xF12,
    TC_PRIMITIVE_UINT32 = 0xF14,
    TC_PRIMITIVE_UINT64 = 0xF18,
    TC_PRIMITIVE_INT8 = 0xE11,
    TC_PRIMITIVE_INT16 = 0xE12,
    TC_PRIMITIVE_INT32 = 0xE14,
    TC_PRIMITIVE_INT64 = 0xE18,
    TC_PRIMITIVE_FLOAT = 0xD14,
    TC_PRIMITIVE_DOUBLE = 0xD18,
    TC_PRIMITIVE_LDOUBLE = 0xD1A
} TcVarPrimitive_t;


/**
 * under virtually any system, signed and unsigned types can be assigned the same way
 * (probably)
 * @param num
 * @param primitive
 * @param buffer
 * @param data
 */
static void tcFillType(TcLength_t num, TcVarPrimitive_t primitive, const uint8_t* buffer, uint8_t* data) {
    switch (primitive) {
        case TC_PRIMITIVE_INT8:
        case TC_PRIMITIVE_UINT8: {
            uint8_t* ptr = data;
            while (num--) {
                *ptr = UINT8VAL(buffer);
                ptr++;
                buffer += 1;
            }
            break;
        }
        case TC_PRIMITIVE_INT16:
        case TC_PRIMITIVE_UINT16: {
            uint16_t* ptr = (uint16_t*) data;
            while (num--) {
                *ptr = UINT16VAL(buffer);
                ptr++;
                buffer += 2;
            }
            break;
        }
        case TC_PRIMITIVE_INT32:
        case TC_PRIMITIVE_UINT32: {
            uint32_t* ptr = (uint32_t*) data;
            while (num--) {
                *ptr = UINT32VAL(buffer);
                ptr++;
                buffer += 4;
            }
            break;
        }

        case TC_PRIMITIVE_INT64:
        case TC_PRIMITIVE_UINT64: {
            uint64_t* ptr = (uint64_t*) data;
            while (num--) {
                *ptr = UINT64VAL(buffer);
                ptr++;
                buffer += 8;
            }
            break;
        }
            // TODO: Implement float and double
        case TC_PRIMITIVE_FLOAT:break;
        case TC_PRIMITIVE_DOUBLE:break;
        case TC_PRIMITIVE_LDOUBLE:break;
    }
}

/**
 * Fills a storage object with the proper value
 * @param node the node
 * @param message the message
 * @param frame the frame
 * @param storage where to fill
 * @param copy Flag indicating if we should copy arrays into this
 */
static uint8_t* tcFillValue(TcNode_t* node, TcMessage_t* message, TcFrameVartype_t* frame, TcValue_t* storage, TcBool_t copy) {
    uint8_t highNibble = (uint8_t) ((frame->type >> 12) & 0xf);
    uint8_t* buffer = frame->data;

    storage->type = frame->type;

    if (highNibble == 0xf || highNibble == 0xe) {
        // internal variable type
        uint16_t primitive = (uint16_t) ((frame->type & 0xf0f) | 0x10);
        uint8_t primitiveType = (uint8_t) ((frame->type >> 8) & 0xf);
        uint8_t elementCount = (uint8_t) ((frame->type >> 4) & 0xf);
        uint8_t primitiveSize = (uint8_t) (frame->type & 0xf);

        if (highNibble == 0xe) {
            // array type
            TcLength_t length = UINT16VAL(buffer);

            TcLength_t totalElements = elementCount * length; // element count should always be 1

            TcLength_t totalBytes = totalElements * primitiveSize;
            buffer += 2;

            storage->store->array.length = length;

            if (!copy) {
                storage->store->array.size = 0; // do I really need this?
                storage->store->array.storage = (void*) buffer; // set the buffer as storage
                //tcFillType(totalElements, (TcVarPrimitive_t) primitive, buffer, buffer); // parse buffer
            } else {
                // the number of elements that fit
                TcLength_t storageElements = (storage->store->array.size / (elementCount * primitiveSize));

                // copy from buffer to the array
                //tcFillType(totalBytes > storage->store->array.size ? storageElements : totalBytes,
                //         (TcVarPrimitive_t) primitive, buffer, storage->store->array.storage);
                memcpy(storage->store->array.storage, buffer, (size_t) (storageElements * primitiveSize + 2));
            }

            buffer += totalBytes;

        } else {
            // we assume the elements are aligned correctly on the union. This *should* work
            //tcFillType(elementCount, (TcVarPrimitive_t) primitive, buffer, storage->store->buffer);

            if (copy) {
                memcpy(storage->store->buffer, buffer, (size_t) (elementCount * primitiveSize));
            } else {
                storage->store->buffer = buffer;
            }

            buffer += elementCount * primitiveSize;
        }
    } else {
        // custom variable type

        // TODO: implement custom variables
    }

    return buffer;
}

TcLength_t tcAssembleMessageVarUpdate(TcNode_t* node, TcConnection_t* connection, TcAddress_t source, TcAddress_t destination, TcVar_t* var) ;

TcBool_t tcFrameHandleVarValue(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) {
    NDEBUG("Handle VarValue");
    const TcVarRequest_t** request = node->varCallbacks;
    TcFrameVartype_t frameVartype;

    uint8_t* data = frame->data;
    uint16_t locationNode = 0;

    if (message->flags & TC_FLAG_ASSUMESOURCE) {
        locationNode = message->source;
    } else {
        locationNode = UINT16VAL(data);
        data += 2;
    }

    uint16_t locationId = UINT16VAL(data);
    data += 2;
    uint16_t varType = UINT16VAL(data);
    data += 2;

    frameVartype.source = locationNode;
    frameVartype.id = locationId;
    frameVartype.header = frame;
    frameVartype.data = data;
    frameVartype.type = varType;

    if (node->root) {
        NDEBUG("Node is root. Checking for update requests");
        const TcVarRequestRoot_t* requestRoot = node->varRequests;

        while (requestRoot->node) {
            if (requestRoot->remote == frameVartype.source && requestRoot->id == frameVartype.id) {
                NDEBUG("Found update request from node %04x", requestRoot->node);

                TcVar_t var;
                TcVarstore_t store;
                var.value.store = &store;
                tcFillValue(node, message, &frameVartype, &var.value, FALSE);

                var.id = requestRoot->id;
                var.value.type = frameVartype.type;

                TcLength_t length = tcAssembleMessageVarUpdate(node, NULL, requestRoot->remote, requestRoot->node, &var);

                NDEBUG("Message made, length: %d", length);
                DEBUGMESSAGE(node->messageBuffer, length);

                tcForward(node, &node->message, NULL, node->messageBuffer);
            }
            requestRoot++;
        }
    }

    while(*request) {
        const TcVarRequest_t* varRequest = *request;

        // check if this matches the request
        if (varRequest->location.id == locationId && varRequest->location.node == locationNode) {
            TcLocation_t location;
            location.node = locationNode;
            location.id = locationId;
            // do we have storage?
            if (varRequest->storage != NULL) {
                tcFillValue(node, message, &frameVartype, varRequest->storage, TRUE);
                varRequest->callback(node, location, *varRequest->storage, varRequest->data);
            } else {
                TcValue_t value;
                TcVarstore_t store;
                value.store = &store;
                tcFillValue(node, message, &frameVartype, &value, FALSE);

                varRequest->callback(node, location, value, varRequest->data);
            }
        }

        request++;
    }

    return TRUE;
}

// called when a service processes it's data
void tcServiceProviderCallback(TcNode_t* node, TcAddress_t source, uint16_t ref, uint8_t status, TcValue_t* value) ;

TcBool_t tcFrameHandleServiceCall(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) {
    // [NODE:16?][REF:16][REMOTE:16][ID:16][ARGN:8][ARGN of [TYPE:16][DATA]]
    NDEBUG("Handle ServiceCall");
    const TcService_t** services = node->services;

    uint8_t* data = frame->data;
    uint16_t sourceNode = 0;

    if (message->flags & TC_FLAG_ASSUMESOURCE) {
        sourceNode = message->source;
    } else {
        sourceNode = UINT16VAL(data);
        data += 2;
    }

    uint16_t ref = UINT16VAL(data);
    data += 2;
    uint16_t remote = UINT16VAL(data);
    data += 2;

    if (remote != node->addr) {
        NDEBUG("Remote doesn't match this node's address");
        return TRUE;
    }

    uint16_t id = UINT16VAL(data);
    data += 2;
    uint8_t argn = UINT8VAL(data);
    data += 1;

    // search for this service
    while(*services) {
        const TcService_t* service = *services;

        if (service->id == id) {
            TcValue_t values[argn];
            TcVarstore_t stores[argn];

            if (argn != service->argn) {
                NDEBUG("Wrong number of arguments for this service: requested %d, frame has %d", service->argn, argn);
                services++;
                continue;
            }

            TcBool_t success = TRUE;

            for (int i = 0; i < argn; i++) {
                uint16_t type = UINT16VAL(data);
                data += 2;

                NDEBUG("Parse argument %d, type %04x", i, type);

                if (type != service->argTypes[i]) {
                    NDEBUG("Wrong argument type!");
                    success = FALSE;
                    break;
                }

                TcFrameVartype_t frameVartype;
                frameVartype.type = type;
                frameVartype.data = data;

                values[i].store = &stores[i];
                data = tcFillValue(node, message, &frameVartype, &values[i], FALSE);
            }

            if (success) {
                service->fn((TcValue_t*) values, sourceNode, ref, &tcServiceProviderCallback);
            }
        }

        services++;
    }

    return TRUE;
}

TcLength_t tcAssembleMessageServiceResponse(TcNode_t* node, TcConnection_t* connection, TcAddress_t source,
                                        TcAddress_t destination, uint16_t ref, uint8_t status, TcValue_t* value) ;

// called when a service processes it's data
void tcServiceProviderCallback(TcNode_t* node, TcAddress_t source, uint16_t ref, uint8_t status, TcValue_t* value) {
    NDEBUG("Callback called with source = %04x, ref = %d", source, ref);

    TcLength_t length = tcAssembleMessageServiceResponse(node, NULL, node->addr, source, ref, status, value);

    NDEBUG("Message made, length: %d", length);
    DEBUGMESSAGE(node->messageBuffer, length);

    tcForward(node, &node->message, NULL, node->messageBuffer);
}

TcBool_t tcFrameHandleServiceResponse(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) {
    // [REF:16][STATUS:8][TYPE:16][DATA]
    NDEBUG("Handle ServiceResponse");
    const TcServiceStore_t* requests = node->serviceStore;

    uint8_t* data = frame->data;

    uint16_t ref = UINT16VAL(data);
    data += 2;
    uint8_t status = UINT8VAL(data);
    data += 1;

    // search for this request
    for (int i = 0; i < node->serviceStoreMax; i++) {
        if (requests[i].ref == ref) {
            // do we have a callback registered?
            if (requests[i].callback) {
                // is there a var value?
                // data start + total length - header length
                if (frame->data + frame->length - 2 > data) {
                    TcFrameVartype_t frameVartype;
                    TcVarstore_t store;
                    TcValue_t value;

                    uint16_t type = UINT16VAL(data);
                    data += 2;

                    frameVartype.type = type;
                    frameVartype.data = data;

                    value.store = &store;
                    data = tcFillValue(node, message, &frameVartype, &value, FALSE);
                    requests[i].callback(node, status, &value, requests[i].data);
                } else {
                    requests[i].callback(node, status, NULL, requests[i].data);
                }
            }
            break;
        }
    }

    return TRUE;
}

TcBool_t tcFrameHandleSerialData(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) {
    // [DESTPORT:16][LEN:16][DATA:LEN*8]
    NDEBUG("Handle SerialData");
    uint8_t* data = frame->data;

    uint16_t port = UINT16VAL(data);
    data += 2;

    TcSerial_t* serial = tcGetSerial(node, port);
    if (!serial) {
        NDEBUG("No such serial port (%04x)", port);
        return TRUE;
    }

    uint16_t len = UINT16VAL(data);
    data += 2;

    NDEBUG("Pushing %d bytes", len);

    while (len--) {
        if (serial->length >= serial->size) {
            NDEBUG("Overflow");
            return FALSE;
        }

        uint8_t c = UINT8VAL(data);
        data++;

        uint16_t pos = (serial->position + serial->length) % serial->size;
        serial->buffer[pos] = c;
        serial->length++;
    }

    DEBUGMESSAGE(serial->buffer, serial->size);

    return TRUE;
}


TcStatus_t tcPushVarRequest(TcNode_t* node, TcVarRequestRoot_t* request) {
    int i;

    for (i = 0; i < node->varRequestsMax - 1; i++) {
        TcVarRequestRoot_t* thisRequest = &node->varRequests[i];
        if (thisRequest->node == 0) {
            thisRequest->node = request->node;
            thisRequest->remote = request->remote;
            thisRequest->id = request->id;

            node->varRequests[i + 1].node = 0;

            return TC_SUCCESS;
        }
    }

    return TC_OVERFLOW;
}

TcBool_t
tcFrameHandleRequestUpdates(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) {
    NDEBUG("Handle UpdateRequest");

    if (node->root != TRUE) {
        // non-root nodes don't care about update requests
        return TRUE;
    }

    TcVarRequestRoot_t request;

    uint8_t* data = frame->data;
    uint16_t locationNode = 0;

    if (message->flags & TC_FLAG_ASSUMESOURCE) {
        locationNode = message->source;
    } else {
        locationNode = UINT16VAL(data);
        data += 2;
    }

    uint16_t remote = UINT16VAL(data);
    data += 2;
    uint16_t id = UINT16VAL(data);
    data += 2;

    NDEBUG("Node %04x wants updates about %04x:%04x", locationNode, remote, id);

    request.node = locationNode;
    request.remote = remote;
    request.id = id;

    tcPushVarRequest(node, &request);

    return TRUE;
}

TcBool_t tcFrameHandleLocalName(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame) {
    // TODO
    NDEBUG("Handle LocalName - Not Implemented");
    return TRUE;
}

TcStatus_t tcFeedMessage(TcNode_t* node, TcConnection_t* source, uint8_t* messageChunk, TcLength_t length) {
    while (length--) {
        TcStatus_t status = tcFeedByte(node, source, *(messageChunk++));

        if (status != TC_SUCCESS) {
            return status;
        }
    }

    return TC_SUCCESS;
}

TcStatus_t tcFeedByte(TcNode_t* node, TcConnection_t* source, uint8_t c) {
    if (source->inBufferPos < source->inBufferLength) {
        source->inBuffer[source->inBufferPos++] = c;

        if (source->inBufferPos >= TC_HEADER_SIZE) {
            tcProcessMessage(node, source);
        }
    } else {
        return TC_OVERFLOW;
    }

    return TC_SUCCESS;
}

TcStatus_t tcOnVarUpdate(TcNode_t* node, const TcVarRequest_t* request) {

    NDEBUG("Requesting callback for variable %04x:%04x", request->location.node, request->location.id);

    TcStatus_t status = tcPush((const void**) node->varCallbacks, request, node->varCallbacksMax, NULL);

    if (status != TC_SUCCESS) {
        return status;
    }

    TcLength_t length = tcAssembleMessageUpdateRequest(node, node->connections[0], request->location.node, request->location.id);

    NDEBUG("Message made, length: %d", length);
    DEBUGMESSAGE(node->messageBuffer, length);

    tcForward(node, &node->message, NULL, node->messageBuffer);

    return status;
}

/**
 * Returns the total storage size for this variable.
 * For arrays, there is an extra 2 bytes of length added
 * @param var the variable
 * @return total bytes
 */
int tcGetTypeSize(TcValue_t* value) {
    uint16_t type =  value->type;
    uint8_t highNibble = (uint8_t) ((type >> 12) & 0xf);

    if (highNibble == 0xf || highNibble == 0xe) {
        // internal variable type
        uint16_t primitive = (uint16_t) ((type & 0xf0f) | 0x10);
        uint8_t primitiveType = (uint8_t) ((type >> 8) & 0xf);
        uint8_t elementCount = (uint8_t) ((type >> 4) & 0xf);
        uint8_t primitiveSize = (uint8_t) (type & 0xf);

        if (highNibble == 0xe) {
            // array type
            TcLength_t length = UINT16VAL(value->store->array.storage);

            TcLength_t totalElements = elementCount * length; // element count should always be 1

            TcLength_t totalBytes = (TcLength_t) (totalElements * primitiveSize + 2);

            return totalBytes;
        }

        return primitiveSize * elementCount;
    }

    // TODO: Implement custom types
    return -1;
}

TcStatus_t tcRegisterVariable(TcNode_t* node, TcVar_t* var) {
    return tcPush((const void**) node->vars, var, node->varsMax, NULL);
}

TcBool_t tcIsArray(TcValue_t* value);

TcLength_t
tcAssembleFrameServiceCall(TcBool_t assumeSource, uint8_t* data, TcNode_t* node, TcAddress_t source, uint16_t ref,
                           TcLocation_t location, uint8_t argn, TcValue_t** values);

void tcUpdateVariable(TcNode_t* node, TcVar_t* var) {
    NDEBUG("Variable updated (length %d) %04x = [%04x]", tcGetTypeSize(&var->value), var->id, var->value.type);
    DEBUGMESSAGE((const uint8_t*) var->value.store->buffer, (TcLength_t) tcGetTypeSize(&var->value));

    TcLength_t length = tcAssembleMessageVarUpdate(node, node->connections[0], node->addr, TC_ROOT, var);

    NDEBUG("Message made, length: %d", length);
    DEBUGMESSAGE(node->messageBuffer, length);

    tcForward(node, &node->message, NULL, node->messageBuffer);

}

TcStatus_t tcRegisterService(TcNode_t* node, TcService_t* service) {
    NDEBUG("Register Service %04x:%04x with %d arguments", node->addr, service->id, service->argn);
    return tcPush((const void**) node->services, service, node->varsMax, NULL);
}

TcLength_t tcAssembleFrameNodeCon(TcBool_t assumeSource, uint8_t* data, TcNode_t* node) {
    if (assumeSource) {
        VALUINT16(data, 3);
        data += 2;
        VALUINT8(data, TC_FRAME_NODECON);
        return 3;
    }

    VALUINT16(data, 5);
    data += 2;
    VALUINT8(data, TC_FRAME_NODECON);
    data += 1;

    VALUINT16(data, node->addr);

    return 5;
}

TcLength_t tcAssembleFrameLocalName(TcBool_t assumeSource, uint8_t* data, TcNode_t* node) {
    // {LENGTH:16}{TYPE:8}[NODE:16?][NAMESIZE:8][NAME:NAMESIZE*8]

    uint8_t len = (uint8_t) strlen(node->name);
    uint16_t fieldLength = (uint16_t) (2 + 1 + (assumeSource ? 0 : 2) + 1 + len);

    VALUINT16(data, fieldLength);
    data += 2;
    VALUINT8(data, TC_FRAME_LOCALNAME);
    data += 1;

    if (!assumeSource) {
        VALUINT16(data, node->addr);
        data += 2;
    }


    *data = len;
    memcpy(data + 1, node->name, len);

    return fieldLength;
}

TcLength_t tcAssembleFrameVarlist(TcBool_t assumeSource, uint8_t* data, TcNode_t* node) {
    // [NODE:16?][NVARS:16][NVARS of [VAR:16][NAMESIZE:8][NAME:NAMESIZE*8]]

    TcLength_t length = 0;

    if (assumeSource) {
        VALUINT16(data, node->addr);
        length += 2;
        data += 2;
    }

    // TODO

    return length;
}

TcLength_t tcAssembleFrameUpdateRequest(TcBool_t assumeSource, uint8_t* data, TcNode_t* node, TcAddress_t remote, TcAddress_t var) {
    // LENGTH:16 + TYPE:8 + [NODE:16?][REMOTE:16][ID:16]
    uint16_t fieldLength = (uint16_t) (2 + 1 + (assumeSource ? 0 : 2) + 2 + 2);

    VALUINT16(data, fieldLength);
    data += 2;
    VALUINT8(data, TC_FRAME_REQUEST_UPDATES);
    data += 1;

    if (!assumeSource) {
        VALUINT16(data, node->addr);
        data += 2;
    }

    VALUINT16(data, remote);
    data += 2;
    VALUINT16(data, var);
    data += 2;

    return fieldLength;
}

TcLength_t tcAssembleFrameVarValue(TcBool_t assumeSource, uint8_t* data, TcNode_t* node, TcAddress_t source, TcVar_t* var) {
    // LENGTH:16 + TYPE:8 + [NODE:16?][ID:16][TYPE:16][DATA]
    int size = tcGetTypeSize(&var->value);
    if (size < 0) { // undefined type
        return 0;
    }
    uint16_t fieldLength = (uint16_t) (2 + 1 + (assumeSource ? 0 : 2) + 2 + 2 + size);

    VALUINT16(data, fieldLength);
    data += 2;
    VALUINT8(data, TC_FRAME_VARVALUE);
    data += 1;

    if (!assumeSource) {
        VALUINT16(data, source);
        data += 2;
    }

    VALUINT16(data, var->id);
    data += 2;
    VALUINT16(data, var->value.type);
    data += 2;

    if (tcIsArray(&var->value)) {
        memcpy(data, var->value.store->array.storage, (size_t) size);
    } else {
        memcpy(data, var->value.store->buffer, (size_t) size);
    }

    return fieldLength;
}

TcLength_t
tcAssembleFrameServiceCall(TcBool_t assumeSource, uint8_t* data, TcNode_t* node, TcAddress_t source, uint16_t ref,
                           TcLocation_t location, uint8_t argn, TcValue_t** values) {
    int size = 0;

    for (int i = 0; i < argn; i++) {
        int tSize = tcGetTypeSize(values[i]);
        if (size < 0) { // undefined type
            return 0;
        }

        size += tSize;
    }

    // LENGTH:16 + TYPE:8 + [NODE:16?][REF:16][REMOTE:16][ID:16][ARGN:8][ARGN of [TYPE:16][DATA]]
    uint16_t fieldLength = (uint16_t) (2 + 1 + (assumeSource ? 0 : 2) + 2 + 2 + 2 + 1 + + argn * 2 + size);

    VALUINT16(data, fieldLength);
    data += 2;
    VALUINT8(data, TC_FRAME_SERVICE_CALL);
    data += 1;

    if (!assumeSource) {
        VALUINT16(data, source);
        data += 2;
    }

    VALUINT16(data, ref);
    data += 2;

    VALUINT16(data, location.node);
    data += 2;

    VALUINT16(data, location.id);
    data += 2;

    VALUINT8(data, argn);
    data += 1;

    for (int i = 0; i < argn; i++) {
        int tSize = tcGetTypeSize(values[i]);

        VALUINT16(data, values[i]->type);
        data += 2;

        if (tcIsArray(values[i])) {
            memcpy(data, values[i]->store->array.storage, (size_t) tSize);
        } else {
            memcpy(data, values[i]->store->buffer, (size_t) tSize);
        }

        data += tSize;
    }

    return fieldLength;
}

TcLength_t
tcAssembleFrameServiceResponse(TcBool_t assumeSource, uint8_t* data, TcNode_t* node, TcAddress_t destination,
                               uint16_t ref, uint8_t status, TcValue_t* value) {
    int size = 0;

    if (value && status) {
        size += tcGetTypeSize(value);
    }

    // LENGTH:16 + TYPE:8 + [REF:16][STATUS:8][TYPE:16][DATA]
    uint16_t fieldLength = (uint16_t) (2 + 1 + 2 + 1 + (size ? (2 + size) : 0));

    VALUINT16(data, fieldLength);
    data += 2;
    VALUINT8(data, TC_FRAME_SERVICE_RESPONSE);
    data += 1;


    VALUINT16(data, ref);
    data += 2;

    VALUINT8(data, status);
    data += 1;

    if (!size) {
        return fieldLength;
    }

    VALUINT16(data, value->type);
    data += 2;

    if (tcIsArray(value)) {
        memcpy(data, value->store->array.storage, (size_t) size);
    } else {
        memcpy(data, value->store->buffer, (size_t) size);
    }

    return fieldLength;
}


TcLength_t tcAssembleFrameSerialData(TcBool_t assumeSource, uint8_t* data, TcNode_t* node, TcLocation_t destination,
                                     uint16_t size, const uint8_t* payload) {

    // LENGTH:16 + TYPE:8 + [DESTPORT:16][LEN:16][DATA:LEN*8]
    uint16_t fieldLength = (uint16_t) (2 + 1 + 2 + 2 + size);

    VALUINT16(data, fieldLength);
    data += 2;
    VALUINT8(data, TC_FRAME_SERIALDATA);
    data += 1;

    VALUINT16(data, destination.id);
    data += 2;

    VALUINT16(data, size);
    data += 2;


    memcpy(data, payload, (size_t) size);

    return fieldLength;
}

TcBool_t tcIsArray(TcValue_t* value) {
    uint16_t type =  value->type;
    uint8_t highNibble = (uint8_t) ((type >> 10) & 0xf);

    if (highNibble == 0xf || highNibble == 0xe) {
        // internal variable type
        if (highNibble == 0xe) { // array type
            return TRUE;
        }
    }

    return FALSE;
}

TcLength_t tcHeaderToBuffer(TcMessage_t* message, uint8_t* buffer) {
    VALUINT16(buffer, message->length);
    buffer += 2;
    VALUINT8(buffer, message->flags);
    buffer += 1;
    VALUINT16(buffer, message->destination);
    buffer += 2;
    VALUINT16(buffer, message->source);
    buffer += 2;
    return TC_HEADER_SIZE;
}

TcLength_t tcAssembleMessageNodeCon(TcNode_t* node, TcConnection_t* connection) {
    node->message.destination = TC_BUBBLE;
    node->message.source = node->addr;
    node->message.flags = TC_FLAG_ASSUMESOURCE;
    node->message.length = TC_HEADER_SIZE;
    node->message.data = node->messageBuffer + TC_HEADER_SIZE;

    uint8_t* data =  node->message.data;

    TcLength_t length = 0;
    TcLength_t frameLength;

    frameLength = tcAssembleFrameNodeCon(TRUE, data, node);
    length += frameLength;
    data += frameLength;

    frameLength = tcAssembleFrameLocalName(TRUE, data, node);
    length += frameLength;
    data += frameLength;

    node->message.length += length;

    tcHeaderToBuffer(&node->message, node->messageBuffer);

    return (TcLength_t) (length + TC_HEADER_SIZE);
}

TcLength_t tcAssembleMessageUpdateRequest(TcNode_t* node, TcConnection_t* connection, TcAddress_t remote, TcAddress_t var) {
    node->message.destination = TC_ROOT;
    node->message.source = node->addr;
    node->message.flags = TC_FLAG_ASSUMESOURCE;
    node->message.length = TC_HEADER_SIZE;
    node->message.data = node->messageBuffer + TC_HEADER_SIZE;

    uint8_t* data =  node->message.data;

    TcLength_t length = 0;
    TcLength_t frameLength;

    frameLength = tcAssembleFrameUpdateRequest(TRUE, data, node, remote, var);
    length += frameLength;
    data += frameLength;


    node->message.length += length;

    tcHeaderToBuffer(&node->message, node->messageBuffer);

    return (TcLength_t) (length + TC_HEADER_SIZE);
}

TcLength_t tcAssembleMessageVarUpdate(TcNode_t* node, TcConnection_t* connection, TcAddress_t source, TcAddress_t destination, TcVar_t* var) {
    TcBool_t assumeSource = source && source == node->addr;
    node->message.destination = destination;
    node->message.source = source ? source : node->addr;
    node->message.flags = (uint8_t) (assumeSource ? TC_FLAG_ASSUMESOURCE : 0);
    node->message.length = TC_HEADER_SIZE;
    node->message.data = node->messageBuffer + TC_HEADER_SIZE;

    NDEBUG("Assemble VarUpdate Message. From %04x (Assume = %d), To %04x", node->message.source, assumeSource, node->message.destination);

    uint8_t* data =  node->message.data;

    TcLength_t length = 0;
    TcLength_t frameLength;

    frameLength = tcAssembleFrameVarValue(assumeSource, data, node, assumeSource ? node->addr : source, var);
    length += frameLength;
    data += frameLength;


    node->message.length += length;

    tcHeaderToBuffer(&node->message, node->messageBuffer);

    return (TcLength_t) (length + TC_HEADER_SIZE);
}

TcLength_t tcAssembleMessageServiceCall(TcNode_t* node, TcConnection_t* connection, TcAddress_t source,
                                        TcLocation_t location, uint16_t ref, uint8_t argn, TcValue_t** values) {
    TcBool_t assumeSource = source && source == node->addr;
    node->message.destination = location.node;
    node->message.source = source ? source : node->addr;
    node->message.flags = (uint8_t) (assumeSource ? TC_FLAG_ASSUMESOURCE : 0);
    node->message.length = TC_HEADER_SIZE;
    node->message.data = node->messageBuffer + TC_HEADER_SIZE;

    NDEBUG("Assemble ServiceCall Message. From %04x (Assume = %d), To %04x", node->message.source, assumeSource,
           node->message.destination);


    uint8_t* data =  node->message.data;

    TcLength_t length = 0;
    TcLength_t frameLength;

    frameLength = tcAssembleFrameServiceCall(assumeSource, data, node, assumeSource ? node->addr : source, ref, location, argn, values);
    length += frameLength;
    data += frameLength;


    node->message.length += length;

    tcHeaderToBuffer(&node->message, node->messageBuffer);

    return (TcLength_t) (length + TC_HEADER_SIZE);
}

TcLength_t tcAssembleMessageServiceResponse(TcNode_t* node, TcConnection_t* connection, TcAddress_t source,
                                        TcAddress_t destination, uint16_t ref, uint8_t status, TcValue_t* value) {
    TcBool_t assumeSource = source && source == node->addr;
    node->message.destination = destination;
    node->message.source = source ? source : node->addr;
    node->message.flags = (uint8_t) (assumeSource ? TC_FLAG_ASSUMESOURCE : 0);
    node->message.length = TC_HEADER_SIZE;
    node->message.data = node->messageBuffer + TC_HEADER_SIZE;

    NDEBUG("Assemble ServiceResponse Message. From %04x (Assume = %d), To %04x", node->message.source, assumeSource,
           node->message.destination);


    uint8_t* data =  node->message.data;

    TcLength_t length = 0;
    TcLength_t frameLength;

    frameLength = tcAssembleFrameServiceResponse(assumeSource, data, node, destination, ref, status, value);
    length += frameLength;
    data += frameLength;


    node->message.length += length;

    tcHeaderToBuffer(&node->message, node->messageBuffer);

    return (TcLength_t) (length + TC_HEADER_SIZE);
}

TcLength_t tcAssembleMessageSerialData(TcNode_t* node, TcConnection_t* connection, TcAddress_t source,
                                       TcLocation_t destination, TcLength_t size, const uint8_t* payload) {
    TcBool_t assumeSource = source && source == node->addr;
    node->message.destination = destination.node;
    node->message.source = source ? source : node->addr;
    node->message.flags = (uint8_t) (assumeSource ? TC_FLAG_ASSUMESOURCE : 0);
    node->message.length = TC_HEADER_SIZE;
    node->message.data = node->messageBuffer + TC_HEADER_SIZE;

    NDEBUG("Assemble SerialData Message. From %04x (Assume = %d), To %04x, Payload size: %d", node->message.source,
           assumeSource, node->message.destination, size);


    uint8_t* data =  node->message.data;

    TcLength_t length = 0;
    TcLength_t frameLength;

    frameLength = tcAssembleFrameSerialData(assumeSource, data, node, destination, size, payload);
    length += frameLength;
    data += frameLength;


    node->message.length += length;

    tcHeaderToBuffer(&node->message, node->messageBuffer);

    return (TcLength_t) (length + TC_HEADER_SIZE);
}

void tcCloneVar(TcValue_t* dest, const TcValue_t* source) {
    TcFrameVartype_t frameVartype;
    frameVartype.type = source->type;
    frameVartype.data = source->store->buffer;

    dest->type = source->type;

    tcFillValue(NULL, NULL, &frameVartype, dest, TRUE);
}

void tcCallService(TcNode_t* node, TcLocation_t service, uint8_t argn, TcValue_t** params, TcServiceCallback_t callback,
                   void* data) {
    NDEBUG("Call service %04x:%04x with %d params", service.node, service.id, argn);

    uint16_t ref = 0xffff;

    for (uint16_t i = 0; i < node->serviceStoreMax; i++) {
        if (node->serviceStore[i].ref == 0xffff) {
            ref = i;
            node->serviceStore[i].ref = i;
            node->serviceStore[i].callback = callback;
            node->serviceStore[i].data = data;
            break;
        }
    }

    NDEBUG("Ref: %04x", ref);

    TcLength_t length = tcAssembleMessageServiceCall(node, NULL, node->addr, service, ref, argn, params);

    NDEBUG("Message made, length: %d", length);
    DEBUGMESSAGE(node->messageBuffer, length);

    tcForward(node, &node->message, NULL, node->messageBuffer);
}

TcStatus_t tcSerialBegin(TcNode_t* node, TcSerial_t* serial) {
    serial->position = 0;
    serial->length = 0;
    return tcPush((const void**) node->serials, serial, node->serialsMax, NULL);
}

TcSerial_t* tcGetSerial(TcNode_t* node, uint16_t id) {
    TcSerial_t** serial = node->serials;

    while (*serial) {
        if ((*serial)->id == id) {
            return *serial;
        }

        serial++;
    }

    return NULL;
}

TcLength_t tcSerialAvailable(TcSerial_t* serial) {
    return (TcLength_t) (serial ? serial->length : 0);
}

uint8_t tcSerialRead(TcSerial_t* serial) {
    if (serial && serial->length) {
        // read from circular buffer
        uint8_t c = serial->buffer[serial->position++];
        serial->length--;
        serial->position %= serial->size;
        return c;
    }

    return 0;

}

void tcSerialSend(TcNode_t* node, TcLocation_t location, TcLength_t size, uint8_t* data) {
    NDEBUG("Send %d bytes to %04x:%04x", size, location.node, location.id);

    TcLength_t length = tcAssembleMessageSerialData(node, NULL, node->addr, location, size, data);

    NDEBUG("Message made, length: %d", length);
    DEBUGMESSAGE(node->messageBuffer, length);

    tcForward(node, &node->message, NULL, node->messageBuffer);
}