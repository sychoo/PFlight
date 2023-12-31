// Generated by the gRPC C++ plugin.
// If you make any local change, they will be lost.
// source: mission_server/mission_server.proto

#include "mission_server/mission_server.pb.h"
#include "mission_server/mission_server.grpc.pb.h"

#include <functional>
#include <grpcpp/impl/codegen/async_stream.h>
#include <grpcpp/impl/codegen/async_unary_call.h>
#include <grpcpp/impl/codegen/channel_interface.h>
#include <grpcpp/impl/codegen/client_unary_call.h>
#include <grpcpp/impl/codegen/client_callback.h>
#include <grpcpp/impl/codegen/message_allocator.h>
#include <grpcpp/impl/codegen/method_handler.h>
#include <grpcpp/impl/codegen/rpc_service_method.h>
#include <grpcpp/impl/codegen/server_callback.h>
#include <grpcpp/impl/codegen/server_callback_handlers.h>
#include <grpcpp/impl/codegen/server_context.h>
#include <grpcpp/impl/codegen/service_type.h>
#include <grpcpp/impl/codegen/sync_stream.h>
namespace mavsdk {
namespace rpc {
namespace mission_server {

static const char* MissionService_method_names[] = {
  "/mavsdk.rpc.mission_server.MissionService/SubscribeIncomingMission",
  "/mavsdk.rpc.mission_server.MissionService/SubscribeCurrentItemChanged",
  "/mavsdk.rpc.mission_server.MissionService/SetCurrentItemComplete",
  "/mavsdk.rpc.mission_server.MissionService/SubscribeClearAll",
};

std::unique_ptr< MissionService::Stub> MissionService::NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options) {
  (void)options;
  std::unique_ptr< MissionService::Stub> stub(new MissionService::Stub(channel));
  return stub;
}

MissionService::Stub::Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel)
  : channel_(channel), rpcmethod_SubscribeIncomingMission_(MissionService_method_names[0], ::grpc::internal::RpcMethod::SERVER_STREAMING, channel)
  , rpcmethod_SubscribeCurrentItemChanged_(MissionService_method_names[1], ::grpc::internal::RpcMethod::SERVER_STREAMING, channel)
  , rpcmethod_SetCurrentItemComplete_(MissionService_method_names[2], ::grpc::internal::RpcMethod::NORMAL_RPC, channel)
  , rpcmethod_SubscribeClearAll_(MissionService_method_names[3], ::grpc::internal::RpcMethod::SERVER_STREAMING, channel)
  {}

::grpc::ClientReader< ::mavsdk::rpc::mission_server::IncomingMissionResponse>* MissionService::Stub::SubscribeIncomingMissionRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeIncomingMissionRequest& request) {
  return ::grpc::internal::ClientReaderFactory< ::mavsdk::rpc::mission_server::IncomingMissionResponse>::Create(channel_.get(), rpcmethod_SubscribeIncomingMission_, context, request);
}

void MissionService::Stub::experimental_async::SubscribeIncomingMission(::grpc::ClientContext* context, ::mavsdk::rpc::mission_server::SubscribeIncomingMissionRequest* request, ::grpc::experimental::ClientReadReactor< ::mavsdk::rpc::mission_server::IncomingMissionResponse>* reactor) {
  ::grpc::internal::ClientCallbackReaderFactory< ::mavsdk::rpc::mission_server::IncomingMissionResponse>::Create(stub_->channel_.get(), stub_->rpcmethod_SubscribeIncomingMission_, context, request, reactor);
}

::grpc::ClientAsyncReader< ::mavsdk::rpc::mission_server::IncomingMissionResponse>* MissionService::Stub::AsyncSubscribeIncomingMissionRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeIncomingMissionRequest& request, ::grpc::CompletionQueue* cq, void* tag) {
  return ::grpc::internal::ClientAsyncReaderFactory< ::mavsdk::rpc::mission_server::IncomingMissionResponse>::Create(channel_.get(), cq, rpcmethod_SubscribeIncomingMission_, context, request, true, tag);
}

::grpc::ClientAsyncReader< ::mavsdk::rpc::mission_server::IncomingMissionResponse>* MissionService::Stub::PrepareAsyncSubscribeIncomingMissionRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeIncomingMissionRequest& request, ::grpc::CompletionQueue* cq) {
  return ::grpc::internal::ClientAsyncReaderFactory< ::mavsdk::rpc::mission_server::IncomingMissionResponse>::Create(channel_.get(), cq, rpcmethod_SubscribeIncomingMission_, context, request, false, nullptr);
}

::grpc::ClientReader< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>* MissionService::Stub::SubscribeCurrentItemChangedRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeCurrentItemChangedRequest& request) {
  return ::grpc::internal::ClientReaderFactory< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>::Create(channel_.get(), rpcmethod_SubscribeCurrentItemChanged_, context, request);
}

void MissionService::Stub::experimental_async::SubscribeCurrentItemChanged(::grpc::ClientContext* context, ::mavsdk::rpc::mission_server::SubscribeCurrentItemChangedRequest* request, ::grpc::experimental::ClientReadReactor< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>* reactor) {
  ::grpc::internal::ClientCallbackReaderFactory< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>::Create(stub_->channel_.get(), stub_->rpcmethod_SubscribeCurrentItemChanged_, context, request, reactor);
}

::grpc::ClientAsyncReader< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>* MissionService::Stub::AsyncSubscribeCurrentItemChangedRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeCurrentItemChangedRequest& request, ::grpc::CompletionQueue* cq, void* tag) {
  return ::grpc::internal::ClientAsyncReaderFactory< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>::Create(channel_.get(), cq, rpcmethod_SubscribeCurrentItemChanged_, context, request, true, tag);
}

::grpc::ClientAsyncReader< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>* MissionService::Stub::PrepareAsyncSubscribeCurrentItemChangedRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeCurrentItemChangedRequest& request, ::grpc::CompletionQueue* cq) {
  return ::grpc::internal::ClientAsyncReaderFactory< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>::Create(channel_.get(), cq, rpcmethod_SubscribeCurrentItemChanged_, context, request, false, nullptr);
}

::grpc::Status MissionService::Stub::SetCurrentItemComplete(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest& request, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse* response) {
  return ::grpc::internal::BlockingUnaryCall< ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse, ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(channel_.get(), rpcmethod_SetCurrentItemComplete_, context, request, response);
}

void MissionService::Stub::experimental_async::SetCurrentItemComplete(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest* request, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse* response, std::function<void(::grpc::Status)> f) {
  ::grpc::internal::CallbackUnaryCall< ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse, ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(stub_->channel_.get(), stub_->rpcmethod_SetCurrentItemComplete_, context, request, response, std::move(f));
}

void MissionService::Stub::experimental_async::SetCurrentItemComplete(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest* request, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse* response, ::grpc::experimental::ClientUnaryReactor* reactor) {
  ::grpc::internal::ClientCallbackUnaryFactory::Create< ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(stub_->channel_.get(), stub_->rpcmethod_SetCurrentItemComplete_, context, request, response, reactor);
}

::grpc::ClientAsyncResponseReader< ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse>* MissionService::Stub::PrepareAsyncSetCurrentItemCompleteRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest& request, ::grpc::CompletionQueue* cq) {
  return ::grpc::internal::ClientAsyncResponseReaderHelper::Create< ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest, ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(channel_.get(), cq, rpcmethod_SetCurrentItemComplete_, context, request);
}

::grpc::ClientAsyncResponseReader< ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse>* MissionService::Stub::AsyncSetCurrentItemCompleteRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest& request, ::grpc::CompletionQueue* cq) {
  auto* result =
    this->PrepareAsyncSetCurrentItemCompleteRaw(context, request, cq);
  result->StartCall();
  return result;
}

::grpc::ClientReader< ::mavsdk::rpc::mission_server::ClearAllResponse>* MissionService::Stub::SubscribeClearAllRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeClearAllRequest& request) {
  return ::grpc::internal::ClientReaderFactory< ::mavsdk::rpc::mission_server::ClearAllResponse>::Create(channel_.get(), rpcmethod_SubscribeClearAll_, context, request);
}

void MissionService::Stub::experimental_async::SubscribeClearAll(::grpc::ClientContext* context, ::mavsdk::rpc::mission_server::SubscribeClearAllRequest* request, ::grpc::experimental::ClientReadReactor< ::mavsdk::rpc::mission_server::ClearAllResponse>* reactor) {
  ::grpc::internal::ClientCallbackReaderFactory< ::mavsdk::rpc::mission_server::ClearAllResponse>::Create(stub_->channel_.get(), stub_->rpcmethod_SubscribeClearAll_, context, request, reactor);
}

::grpc::ClientAsyncReader< ::mavsdk::rpc::mission_server::ClearAllResponse>* MissionService::Stub::AsyncSubscribeClearAllRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeClearAllRequest& request, ::grpc::CompletionQueue* cq, void* tag) {
  return ::grpc::internal::ClientAsyncReaderFactory< ::mavsdk::rpc::mission_server::ClearAllResponse>::Create(channel_.get(), cq, rpcmethod_SubscribeClearAll_, context, request, true, tag);
}

::grpc::ClientAsyncReader< ::mavsdk::rpc::mission_server::ClearAllResponse>* MissionService::Stub::PrepareAsyncSubscribeClearAllRaw(::grpc::ClientContext* context, const ::mavsdk::rpc::mission_server::SubscribeClearAllRequest& request, ::grpc::CompletionQueue* cq) {
  return ::grpc::internal::ClientAsyncReaderFactory< ::mavsdk::rpc::mission_server::ClearAllResponse>::Create(channel_.get(), cq, rpcmethod_SubscribeClearAll_, context, request, false, nullptr);
}

MissionService::Service::Service() {
  AddMethod(new ::grpc::internal::RpcServiceMethod(
      MissionService_method_names[0],
      ::grpc::internal::RpcMethod::SERVER_STREAMING,
      new ::grpc::internal::ServerStreamingHandler< MissionService::Service, ::mavsdk::rpc::mission_server::SubscribeIncomingMissionRequest, ::mavsdk::rpc::mission_server::IncomingMissionResponse>(
          [](MissionService::Service* service,
             ::grpc::ServerContext* ctx,
             const ::mavsdk::rpc::mission_server::SubscribeIncomingMissionRequest* req,
             ::grpc::ServerWriter<::mavsdk::rpc::mission_server::IncomingMissionResponse>* writer) {
               return service->SubscribeIncomingMission(ctx, req, writer);
             }, this)));
  AddMethod(new ::grpc::internal::RpcServiceMethod(
      MissionService_method_names[1],
      ::grpc::internal::RpcMethod::SERVER_STREAMING,
      new ::grpc::internal::ServerStreamingHandler< MissionService::Service, ::mavsdk::rpc::mission_server::SubscribeCurrentItemChangedRequest, ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>(
          [](MissionService::Service* service,
             ::grpc::ServerContext* ctx,
             const ::mavsdk::rpc::mission_server::SubscribeCurrentItemChangedRequest* req,
             ::grpc::ServerWriter<::mavsdk::rpc::mission_server::CurrentItemChangedResponse>* writer) {
               return service->SubscribeCurrentItemChanged(ctx, req, writer);
             }, this)));
  AddMethod(new ::grpc::internal::RpcServiceMethod(
      MissionService_method_names[2],
      ::grpc::internal::RpcMethod::NORMAL_RPC,
      new ::grpc::internal::RpcMethodHandler< MissionService::Service, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse, ::grpc::protobuf::MessageLite, ::grpc::protobuf::MessageLite>(
          [](MissionService::Service* service,
             ::grpc::ServerContext* ctx,
             const ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest* req,
             ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse* resp) {
               return service->SetCurrentItemComplete(ctx, req, resp);
             }, this)));
  AddMethod(new ::grpc::internal::RpcServiceMethod(
      MissionService_method_names[3],
      ::grpc::internal::RpcMethod::SERVER_STREAMING,
      new ::grpc::internal::ServerStreamingHandler< MissionService::Service, ::mavsdk::rpc::mission_server::SubscribeClearAllRequest, ::mavsdk::rpc::mission_server::ClearAllResponse>(
          [](MissionService::Service* service,
             ::grpc::ServerContext* ctx,
             const ::mavsdk::rpc::mission_server::SubscribeClearAllRequest* req,
             ::grpc::ServerWriter<::mavsdk::rpc::mission_server::ClearAllResponse>* writer) {
               return service->SubscribeClearAll(ctx, req, writer);
             }, this)));
}

MissionService::Service::~Service() {
}

::grpc::Status MissionService::Service::SubscribeIncomingMission(::grpc::ServerContext* context, const ::mavsdk::rpc::mission_server::SubscribeIncomingMissionRequest* request, ::grpc::ServerWriter< ::mavsdk::rpc::mission_server::IncomingMissionResponse>* writer) {
  (void) context;
  (void) request;
  (void) writer;
  return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status MissionService::Service::SubscribeCurrentItemChanged(::grpc::ServerContext* context, const ::mavsdk::rpc::mission_server::SubscribeCurrentItemChangedRequest* request, ::grpc::ServerWriter< ::mavsdk::rpc::mission_server::CurrentItemChangedResponse>* writer) {
  (void) context;
  (void) request;
  (void) writer;
  return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status MissionService::Service::SetCurrentItemComplete(::grpc::ServerContext* context, const ::mavsdk::rpc::mission_server::SetCurrentItemCompleteRequest* request, ::mavsdk::rpc::mission_server::SetCurrentItemCompleteResponse* response) {
  (void) context;
  (void) request;
  (void) response;
  return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status MissionService::Service::SubscribeClearAll(::grpc::ServerContext* context, const ::mavsdk::rpc::mission_server::SubscribeClearAllRequest* request, ::grpc::ServerWriter< ::mavsdk::rpc::mission_server::ClearAllResponse>* writer) {
  (void) context;
  (void) request;
  (void) writer;
  return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}


}  // namespace mavsdk
}  // namespace rpc
}  // namespace mission_server

