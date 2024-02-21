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
