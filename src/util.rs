use std::mem;

/// Wraps a `Vec<T>` to communicate
/// that it is being used exclusively
/// as a stack
///
/// Most uses of a stack will want to pop
/// in reverse of the original inserton order
/// (like, say, processing a token stream)
pub struct Stack<T> {

    inner: Vec<T>,
}

impl<T> Stack<T> {

    pub fn new() -> Stack<T> {
        Stack { inner: Vec::new(), }
    }

    pub fn from_vec(mut vec: Vec<T>) -> Stack<T> {
        {
            let len = vec.len();
            let (front, back) = vec.split_at_mut((len + 1) / 2);
            let back_len = back.len();
            for i in 0..back_len {
                mem::swap(&mut front[i], &mut back[back_len - i - 1])
            }
        }
        Stack { inner: vec, }
    }

    pub fn push(&mut self, x: T) {
        self.inner.push(x);
    }

    pub fn peek(&mut self) -> Option<&T> {
        self.inner.last()
    }

    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.inner.last_mut()
    }

    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

}