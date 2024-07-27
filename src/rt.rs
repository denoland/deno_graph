// Copyright 2018-2024 the Deno authors. MIT license.

use futures::channel::oneshot;
use std::future::Future;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

pub type BoxedFuture = Pin<Box<dyn Future<Output = ()> + 'static>>;

/// An executor for futures.
///
/// This trait allows deno_graph to run background tasks on
/// the async executor.
pub trait Executor {
  /// Spawns a future to run on this executor.
  fn execute(&self, fut: BoxedFuture) -> BoxedFuture;
}

impl<'a> Default for &'a dyn Executor {
  fn default() -> &'a dyn Executor {
    {
      struct DefaultExecutor;

      impl Executor for DefaultExecutor {
        fn execute(&self, future: BoxedFuture) -> BoxedFuture {
          #[cfg(target_arch = "wasm32")]
          return future;

          #[cfg(not(target_arch = "wasm32"))]
          {
            use futures::FutureExt;
            deno_unsync::spawn(future).map(|v| v.unwrap()).boxed_local()
          }
        }
      }

      &DefaultExecutor
    }
  }
}

pub(crate) struct JoinHandle<T> {
  rx: oneshot::Receiver<T>,
  fut: BoxedFuture,
}

impl<T> Future for JoinHandle<T> {
  type Output = T;

  fn poll(mut self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
    if let Poll::Ready(()) = Pin::new(&mut self.fut).poll(cx) {
      if let Poll::Ready(Ok(res)) = Pin::new(&mut self.rx).poll(cx) {
        Poll::Ready(res)
      } else {
        panic!("task panic");
      }
    } else {
      Poll::Pending
    }
  }
}

pub(crate) fn spawn<F, T: 'static>(
  executor: &dyn Executor,
  f: F,
) -> JoinHandle<T>
where
  F: Future<Output = T> + 'static,
{
  let (tx, rx) = oneshot::channel();
  let fut = executor.execute(Box::pin(async move {
    tx.send(f.await).ok();
  }));

  JoinHandle { rx, fut }
}
