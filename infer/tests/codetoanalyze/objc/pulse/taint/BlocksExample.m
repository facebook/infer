/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#import <Foundation/Foundation.h>

@interface BlocksExample : NSObject
@end

@interface Mailbox : NSObject
@end

@interface MailboxSessionBind : NSObject
@property(nonatomic, assign) Mailbox* mailbox;
@end

@implementation MailboxSessionBind {
}

- (void)boundMailbox:(void (^)(Mailbox*))completion {
  completion(self.mailbox);
}

@end

@interface ThreadListManager : NSObject

- (void)loadModelListWithCompletion:(void (^)())callback;

@end

@implementation ThreadListManager

- (void)loadModelListWithCompletion:(void (^)())callback {
  callback();
}

@end

@interface ThreadListDataSource : NSObject {
 @public
  Mailbox* _mailbox;
  ThreadListManager* _threadListManager;
  void (^_updateCallback)(void);
}

- (instancetype)initWithMailbox:(Mailbox*)mailbox
                 updateCallback:(void (^)(void))updateCallback;

@end

@implementation ThreadListDataSource

- (instancetype)initWithMailbox:(Mailbox*)mailbox
                 updateCallback:(void (^)(void))updateCallback {
  if (self = [super init]) {
    _mailbox = mailbox;
    _updateCallback = updateCallback;
  }
  return self;
}

- (void)startThreadListUpdates {
  ThreadListManager* threadListManager = _threadListManager;
  __weak __typeof(self) weakSelf = self;
  [_threadListManager loadModelListWithCompletion:^{
    __typeof(self) strongSelf = weakSelf;
    if (strongSelf) {
      strongSelf->_updateCallback();
    }
  }];
}

@end

id ThreadListDataSourceForMailbox(Mailbox* mailbox,
                                  void (^updateCallback)(void)) {
  ThreadListDataSource* dataSource =
      [[ThreadListDataSource alloc] initWithMailbox:mailbox
                                     updateCallback:updateCallback];
  [dataSource startThreadListUpdates];
  return dataSource;
}

@interface ThreadListViewController : NSObject
@property(nonatomic, assign) ThreadListDataSource* dataSource;
@end

@implementation ThreadListViewController

+ (ThreadListViewController*)ThreadListViewControllerWithMailbox:
    (Mailbox*)mailbox {
  ThreadListViewController* controller = [self new];
  __weak ThreadListViewController* weakController = controller;
  controller.dataSource = ThreadListDataSourceForMailbox(mailbox, ^{
    [weakController _updateThreadList];
  });
  return controller;
}

- (void)_updateThreadList {
}

@end

@interface MailboxTaint : NSObject
+ (Mailbox*)source;
+ (void)sink:(Mailbox*)mailbox;
@end

@implementation MailboxTaint

+ (Mailbox*)source {
  return [Mailbox new];
}

+ (void)sink:(Mailbox*)mailbox {
}

@end

void ActivateMailbox(Mailbox* mailbox) { [MailboxTaint sink:mailbox]; }

void CloseMailbox(void (^callback)(void)) { callback(); }

id SessionScopedObjectForClass(Class klass);

@implementation BlocksExample

- (void)example1_bad {
  dispatch_async(dispatch_get_main_queue(), ^{
    MailboxSessionBind* mailboxSessionBind =
        SessionScopedObjectForClass([MailboxSessionBind class]);
    mailboxSessionBind.mailbox = MailboxTaint.source;
    [mailboxSessionBind boundMailbox:^(Mailbox* boundMailbox) {
      ActivateMailbox(boundMailbox);
    }];
  });
}

- (void)_presentThreadListViewForMailbox:(Mailbox*)mailbox {
  ThreadListViewController* controller =
      [ThreadListViewController ThreadListViewControllerWithMailbox:mailbox];
  [MailboxTaint sink:controller.dataSource->_mailbox];
}

- (void)example2_bad {
  dispatch_async(dispatch_get_main_queue(), ^{
    MailboxSessionBind* mailboxSessionBind =
        SessionScopedObjectForClass([MailboxSessionBind class]);
    mailboxSessionBind.mailbox = MailboxTaint.source;
    [mailboxSessionBind boundMailbox:^(Mailbox* boundMailbox) {
      CloseMailbox(^{
        [self _presentThreadListViewForMailbox:boundMailbox];
      });
    }];
  });
}

@end
