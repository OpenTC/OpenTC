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

#ifndef OPENTC_H
#define OPENTC_H

#include <stdint.h>
#include <stdbool.h>

#ifndef TC_CONF_MAX_VAR_LENGTH
//# "You need to define TC_CONF_MAX_VAR_LENGTH"
#endif // !TC_CONF_MAX_VAR_LENGTH

#ifndef TRUE
#define TRUE true
#endif

#ifndef FALSE
#define FALSE false
#endif

// the escape value constant
#define TC_ESCAPE 0x33

// end of transmission constant
#define TC_EOT    0x04

// if you want to transmit byte 0x33
// it should be sent over the wire as
// 0x33 0x33, so it's escaped

// setting address to this constant makes
// a message "bubble up" the tree
// this is used while announcing
#define TC_BUBBLE 0xfffe

#define TC_ROOT 0xffff

/*******************************/
/*     VAR TYPES DEFINES       */
/*******************************/
// in general, the var types are structured so that the last nibble
// represents the number of bytes of the primitive, the second-to-last
// is the number of primitives, and the next one is the primitive type
//
// Types:
//     F = UINT
//     E = INT
//     D = Floating point
//
// Values beginning with 0xF and 0xE are reserved for predefined types. DO NOT
// make custom types that start with 0xF or 0xE
enum {
	// unsigned integer basic types
	TC_VARTYPE_UINT8 = 0xFF11,
	TC_VARTYPE_UINT16 = 0xFF12,
	TC_VARTYPE_UINT32 = 0xFF14,
	TC_VARTYPE_UINT64 = 0xFF18,
	TC_VARTYPE_UINT8VEC2 = 0xFF21,
	TC_VARTYPE_UINT8VEC3 = 0xFF31,
	TC_VARTYPE_UINT16VEC2 = 0xFF22,
	TC_VARTYPE_UINT16VEC3 = 0xFF32,
	TC_VARTYPE_UINT32VEC2 = 0xFF24,
	TC_VARTYPE_UINT32VEC3 = 0xFF34,

	// signed integer basic types
	TC_VARTYPE_INT8 = 0xFE11,
	TC_VARTYPE_INT16 = 0xFE12,
	TC_VARTYPE_INT32 = 0xFE14,
	TC_VARTYPE_INT64 = 0xFE18,
	TC_VARTYPE_INT8VEC2 = 0xFE21,
	TC_VARTYPE_INT8VEC3 = 0xFE31,
	TC_VARTYPE_INT16VEC2 = 0xFE22,
	TC_VARTYPE_INT16VEC3 = 0xFE32,
	TC_VARTYPE_INT32VEC2 = 0xFE24,
	TC_VARTYPE_INT32VEC3 = 0xFE34,

	// floating point basic types
	TC_VARTYPE_FLOAT = 0xFD14,
	TC_VARTYPE_DOUBLE = 0xFD18,
	TC_VARTYPE_LDOUBLE = 0xFD1A,
	TC_VARTYPE_FLOATVEC2 = 0xFD24,
	TC_VARTYPE_FLOATVEC3 = 0xFD34,
	TC_VARTYPE_DOUBLEVEC2 = 0xFD28,
	TC_VARTYPE_DOUBLEVEC3 = 0xFD38,

	// ARR types
	// all ARR types begin with an uint16_t length field
	TC_VARTYPE_UINT8ARR = 0xEF11,
    TC_VARTYPE_UINT16ARR = 0xEF12,
    TC_VARTYPE_UINT32ARR = 0xEF14,
    TC_VARTYPE_UINT64ARR = 0xEF18,
    TC_VARTYPE_INT8ARR = 0xEE11,
    TC_VARTYPE_INT16ARR = 0xEE12,
    TC_VARTYPE_INT32ARR = 0xEE14,
    TC_VARTYPE_INT64ARR = 0xEE18,
	TC_VARTYPE_FLOATARR = 0xED14,
	TC_VARTYPE_DOUBLEARR = 0xED18,
	TC_VARTYPE_LDOUBLEARR = 0xED1A
};

typedef uint16_t TcVartype_t;

typedef union {
    struct {
        uint8_t* storage; // note: this will contain the length field, use tcGetArray* functions to manipulate
        uint16_t length;
        uint16_t size;
    } array;

    uint8_t* buffer;

} TcVarstore_t;

/*******************************/
/*     FRAME TYPES DEFINES     */
/*******************************/

enum {
	// [NODE:16?]
	TC_FRAME_NODECON = 0xCC,

	// [NODE:16?][NVARS:16][NVARS of [VAR:16][NAMESIZE:8][NAME:NAMESIZE*8]]
	TC_FRAME_CVARLIST = 0x03,

	// [NODE:16?][NAMESIZE:8][NAME:NAMESIZE*8]
	TC_FRAME_LOCALNAME = 0x09,

	// [NODE:16?][ID:16][TYPE:16][DATA]
	TC_FRAME_VARVALUE = 0xFF,

    // [DESTPORT:16][LEN:16][DATA:LEN*8]
    TC_FRAME_SERIALDATA = 0x14,

    // [NODE:16?][REMOTE:16][ID:16]
    TC_FRAME_REQUEST_UPDATES = 0xEE,

    // [NODE:16?][REF:16][REMOTE:16][ID:16][ARGN:8][ARGN of [TYPE:16][DATA]]
    TC_FRAME_SERVICE_CALL = 0x20,

    // [REF:16][STATUS:8][TYPE:16][DATA]
    TC_FRAME_SERVICE_RESPONSE = 0x21
};


///*******************************/
///*    CONTROL TYPES DEFINES    */
///*******************************/
//
//enum {
//    // [SOURCEPORT:16][DESTPORT:16][LEN:8][DATA:LEN*8]
//    TC_CONTROL_SERIALDATA = 0x14
//};

/*******************************/
/*           FLAGS             */
/*******************************/

// setting this flags replaces all [NODE] frame fields with the SOURCEADDR
// and allows them to not be transmitted
#define TC_FLAG_ASSUMESOURCE  0x01

//// setting this flag defines this as a control packet
//#define TC_FLAG_CONTROL       0x80

typedef uint16_t TcLength_t;
typedef bool TcBool_t;
typedef uint16_t TcAddress_t;
typedef uint8_t TcFrameType_t;

typedef struct {
	// length including the length field
	TcLength_t length;

	// the TC_FRAME type
    TcFrameType_t type;

	// the data
	uint8_t* data;
} TcFrame_t;

typedef struct {
    TcFrame_t* header;
    uint16_t source;
    uint16_t id;
    TcVartype_t type;
    uint8_t* data;
} TcFrameVartype_t;

//typedef struct {
//	TcLength_t length;
//
//	uint8_t type;
//
//	uint8_t* data;
//} TcControl_t;

typedef struct {
	// header + payload size
	TcLength_t length;

	// [CONTROL:1][UNUSED:6][ASSUME:1]
	uint8_t  flags;

	// the destination is the first field
	// so nodes can quickly decide
	// where a message is headed, and start
	// transmitting it as soon as possible
	TcAddress_t destination;

	// the source address
	TcAddress_t source;

	uint8_t* data;
} TcMessage_t;

// header length in bytes
#define TC_HEADER_SIZE 7

typedef struct TcConnection_s TcConnection_t;
typedef struct TcNode_s TcNode_t;

typedef void(*TcConnectionSend_t)(TcNode_t* node, TcConnection_t* connection, uint8_t* data, TcLength_t length);

struct TcConnection_s {
	// a null-terminated vector of node ids that this connection
	// provides access to.
	TcAddress_t* children;

	// the maximum number of children (max length of *children)
	TcLength_t childrenMax;

	// the incomming message buffer
	uint8_t* inBuffer;

	// the buffer size
	TcLength_t inBufferLength;

	// current position in the buffer
	TcLength_t inBufferPos;

	// the position of this connection in the node's connection array
	uint8_t id;

	TcConnectionSend_t handler;
};

typedef struct {
	// type (see TC_VARTYPE_*)
	uint16_t type;

	// pointer to the value
	TcVarstore_t* store;
} TcValue_t;

// this type represents a variable
// the local ID field is the variable's "name"
typedef struct {
	// local ID
	uint16_t id;

	TcValue_t value;
} TcVar_t;


// adds the remote address to the variable, making it
// an absolute address
typedef struct {
	// used to represent a remote variable
	TcAddress_t node;

	// the var value
	TcVar_t var;
} TcRemoteVar_t;

typedef struct TcLocation_s {
    // node id
    uint16_t node;

    // local id
    uint16_t id;
} TcLocation_t;

typedef void(*TcVarCallback_t)(TcNode_t* node, TcLocation_t location, TcValue_t var, void* data);

// this function is provided by the requester, and is called when a response arrives
// result can be NULL if the service is void or status == 0
typedef void(*TcServiceCallback_t)(TcNode_t* node, uint8_t status, TcValue_t* result, void* data);

// this is passed to the service, and should be called when the response is ready
// @param source is the address of the client who requested this
// @param ref a unique value representing this call in the client
// @param status should be TRUE or FALSE, meaning success or failure
// @param value the return value
typedef void(*TcServiceProviderCallback_t)(TcNode_t* node, TcAddress_t source, uint16_t ref, uint8_t status, TcValue_t* value);

// the executor function of a service provider
typedef void(*TcServiceProvider_t)(TcValue_t* args, TcAddress_t source, uint16_t ref, TcServiceProviderCallback_t callback);

typedef TcBool_t(*TcFrameHandler_t)(TcNode_t* node, TcConnection_t* connection, TcMessage_t* message, TcFrame_t* frame);
typedef TcBool_t(*TcVartypeHandler_t)(TcNode_t* node, TcMessage_t* message, TcFrameVartype_t* frame, TcValue_t* storage, TcBool_t copy);

typedef void(*TcObserver_t)(TcNode_t* node, TcMessage_t* message);

typedef struct {
    TcFrameType_t type;
    TcFrameHandler_t handler;
} TcCustomFrameHandler_t;

typedef struct {
    TcVartype_t type;
    TcVartypeHandler_t handler;
} TcCustomVartypeHandler_t;

// represents a callback request
typedef struct {
    TcLocation_t location;
    TcValue_t* storage;
    TcVarCallback_t callback;
    void* data;
} TcVarRequest_t;

// used by the root node to store var requests
typedef struct {
    // node that wants the value
    TcAddress_t node;

    // variable node
    TcAddress_t remote;

    // variable id
    TcAddress_t id;
} TcVarRequestRoot_t;

typedef struct {
	// local id
	uint16_t id;

	// number of arguments
	uint8_t argn;

	// `argn` argument types
	uint16_t* argTypes;

	// return type
	uint16_t returnType;

	// processing function
	TcServiceProvider_t fn;
} TcService_t;

typedef struct {
    uint16_t ref;
    TcServiceCallback_t callback;
    void* data;
} TcServiceStore_t;

typedef struct {
    // the id acts as a "port" for this connection
    uint16_t id;

    // the buffer
    uint8_t* buffer;

    // buffer size
    TcLength_t size;

    // buffer position
    TcLength_t position;

    // buffer length
    TcLength_t length;
} TcSerial_t;

struct TcNode_s {
	// the name of this node
	const char* name;

	// the address of this node
	TcAddress_t addr;

	// null-terminated list of connections
	// connections[0] is the parent connection
	// if this is not the root
	TcConnection_t** connections;

	TcLength_t connectionsMax;

	// if this is true, this node is the root
	TcBool_t root;

	// the variables associated with this node
	TcVar_t** vars;

	TcLength_t varsMax;

	// services this node provides
	const TcService_t** services;

	TcLength_t servicesMax;

	// buffer used to build messages
	uint8_t* messageBuffer;

	TcLength_t messageBufferLength;

    // null-terminated list of custom frame handlers
    TcCustomFrameHandler_t* customHandlers;

    // null-terminated list of var callbacks
    const TcVarRequest_t** varCallbacks;

    // the max length of the array above
    TcLength_t varCallbacksMax;

    // null-terminated array of custom handlers
    const TcCustomVartypeHandler_t* customVartypeHandlers;

    // used by the root to store var requests (terminator has node == 0)
    TcVarRequestRoot_t* varRequests;

    // max length of var requests
    TcLength_t varRequestsMax;

    // used to store calls to remote services (unused has ref == 0xFFFF)
    TcServiceStore_t* serviceStore;

    // max number of service calls
    TcLength_t serviceStoreMax;

    // null-terminated list of serial connections
    TcSerial_t** serials;

    // max number of serial connections (length of serials)
    TcLength_t serialsMax;

    // optional observer that is called on every message
    TcObserver_t observer;

	/* Internal Fields */

	// used to build a message
	TcMessage_t message;
};

typedef enum TcStatus_s {
	TC_SUCCESS,
	TC_ERROR,
	TC_OVERFLOW,
	TC_FIELD_TOO_BIG
} TcStatus_t;

/*******************************/
/*      HELPER FUNCTIONS       */
/*******************************/

// counts null-terminated fields
TcLength_t tcNumElements(const void** elem);

#define TC_NUM_ELEMENTS(x) tcNumElements((const void**) (x))

// adds an element to the end of a null-terminated array
TcStatus_t tcPush(const void** container, const void* elem, TcLength_t maxLength, TcLength_t * index);

/*******************************/
/*       MAIN FUNCTIONS        */
/*******************************/

// initializes the static structures
void tcInit();

// creates a node
TcStatus_t tcInitNode(TcNode_t* node);
TcStatus_t tcInitConnection(TcNode_t* node, TcConnection_t* conn);

// feeds a message into a connection
TcStatus_t tcFeedMessage(TcNode_t* node, TcConnection_t* source, uint8_t* messageChunk, TcLength_t length);

// feeds a byte into a connection
TcStatus_t tcFeedByte(TcNode_t* node, TcConnection_t* source, uint8_t c);

/*******************************/
/*      SERVICE FUNCTIONS      */
/*******************************/

// register this service on the node's list
TcStatus_t tcRegisterService(TcNode_t* node, TcService_t* service);

// call a remote service and wait for the result
// if storage is null, a static value will be returned, and
// should be considered volatile.
void tcCallService(TcNode_t* node, TcLocation_t service, uint8_t argn, TcValue_t** params, TcServiceCallback_t callback,
                   void* data);

/*******************************/
/*      VARIABLE FUNCTIONS     */
/*******************************/

void tcInitVar(TcNode_t* node, TcVar_t* var);

void tcCloneVar(TcValue_t* dest, const TcValue_t* source);

// registers a callback for a remote variable update
// note that this implicitly requests updates for this node
// if storage is null, a static value will be returned, and
// should be considered volatile (it may change after the function returns)
TcStatus_t tcOnVarUpdate(TcNode_t* node, const TcVarRequest_t* request);

// register this variable on this nodes list
TcStatus_t tcRegisterVariable(TcNode_t* node, TcVar_t* var);

// update a variable
void tcUpdateVariable(TcNode_t* node, TcVar_t* var);

/*******************************/
/*       SERIAL FUNCTIONS      */
/*******************************/

// store this serial object
TcStatus_t tcSerialBegin(TcNode_t* node, TcSerial_t* serial);

// get the serial handle
TcSerial_t* tcGetSerial(TcNode_t* node, uint16_t id);

// returns the number of byte available
TcLength_t tcSerialAvailable(TcSerial_t* serial);

// read a byte
uint8_t tcSerialRead(TcSerial_t* serial);

// send data
void tcSerialSend(TcNode_t* node, TcLocation_t location, TcLength_t size, uint8_t* data);

/*******************************/
/*       ACCESS FUNCTIONS      */
/*******************************/

void tcSetVec8(uint8_t* buffer, uint8_t at, uint8_t val);
void tcSetVec16(uint8_t* buffer, uint8_t at, uint16_t val);
void tcSetVec32(uint8_t* buffer, uint8_t at, uint32_t val);
void tcSetVec64(uint8_t* buffer, uint8_t at, uint64_t val);
uint8_t tcGetVec8(const uint8_t* buffer, uint8_t at);
uint16_t tcGetVec16(const uint8_t* buffer, uint8_t at);
uint32_t tcGetVec32(const uint8_t* buffer, uint8_t at);
uint64_t tcGetVec64(const uint8_t* buffer, uint8_t at);

void tcSetArray8(TcVarstore_t* var, uint8_t at, uint8_t val);
void tcSetArray16(TcVarstore_t* var, uint8_t at, uint16_t val);
void tcSetArray32(TcVarstore_t* var, uint8_t at, uint32_t val);
void tcSetArray64(TcVarstore_t* var, uint8_t at, uint64_t val);
uint8_t tcGetArray8(TcVarstore_t* var, uint8_t at);
uint16_t tcGetArray16(TcVarstore_t* var, uint8_t at);
uint32_t tcGetArray32(TcVarstore_t* var, uint8_t at);
uint64_t tcGetArray64(TcVarstore_t* var, uint8_t at);
void tcArraySetLength(TcVarstore_t* var, uint16_t length);

#endif // !OPENTC_H
