use crate::*;

impl Executor {
    ///
    /// Execute merge sort for Vec of *Value*s.
    ///
    pub(crate) fn sort_by<F>(
        &mut self,
        globals: &mut Globals,
        vec: &mut [Value],
        mut compare: F,
    ) -> Result<()>
    where
        F: FnMut(&mut Executor, &mut Globals, Value, Value) -> Result<std::cmp::Ordering>,
    {
        self.merge_sort(globals, vec, |vm, globals, a, b| {
            Ok(compare(vm, globals, a, b)? == std::cmp::Ordering::Less)
        })
    }

    fn merge_sort<F>(
        &mut self,
        globals: &mut Globals,
        v: &mut [Value],
        mut is_less: F,
    ) -> Result<()>
    where
        F: FnMut(&mut Executor, &mut Globals, Value, Value) -> Result<bool>,
    {
        // Slices of up to this length get sorted using insertion sort.
        const MAX_INSERTION: usize = 20;
        // Very short runs are extended using insertion sort to span at least this many elements.
        const MIN_RUN: usize = 10;

        let len = v.len();

        // Short arrays get sorted in-place via insertion sort to avoid allocations.
        if len <= MAX_INSERTION {
            if len >= 2 {
                for i in (0..len - 1).rev() {
                    self.insert_head(globals, &mut v[i..], &mut is_less)?;
                }
            }
            return Ok(());
        }

        let mut buf = Vec::with_capacity(len / 2);
        let mut runs = vec![];
        let mut end = len;
        while end > 0 {
            // Find the next natural run, and reverse it if it's strictly descending.
            let mut start = end - 1;
            if start > 0 {
                start -= 1;
                unsafe {
                    if is_less(
                        self,
                        globals,
                        *v.get_unchecked(start + 1),
                        *v.get_unchecked(start),
                    )? {
                        while start > 0
                            && is_less(
                                self,
                                globals,
                                *v.get_unchecked(start),
                                *v.get_unchecked(start - 1),
                            )?
                        {
                            start -= 1;
                        }
                        v[start..end].reverse();
                    } else {
                        while start > 0
                            && !is_less(
                                self,
                                globals,
                                *v.get_unchecked(start),
                                *v.get_unchecked(start - 1),
                            )?
                        {
                            start -= 1;
                        }
                    }
                }
            }

            // Insert some more elements into the run if it's too short. Insertion sort is faster than
            // merge sort on short sequences, so this significantly improves performance.
            while start > 0 && end - start < MIN_RUN {
                start -= 1;
                self.insert_head(globals, &mut v[start..end], &mut is_less)?;
            }

            // Push this run onto the stack.
            runs.push(Run {
                start,
                len: end - start,
            });
            end = start;

            // Merge some pairs of adjacent runs to satisfy the invariants.
            while let Some(r) = collapse(&runs) {
                let left = runs[r + 1];
                let right = runs[r];
                self.merge(
                    globals,
                    &mut v[left.start..right.start + right.len],
                    left.len,
                    buf.as_mut_ptr(),
                    &mut is_less,
                )?;
                runs[r] = Run {
                    start: left.start,
                    len: left.len + right.len,
                };
                runs.remove(r + 1);
            }
        }

        debug_assert!(runs.len() == 1 && runs[0].start == 0 && runs[0].len == len);

        return Ok(());

        #[inline]
        fn collapse(runs: &[Run]) -> Option<usize> {
            let n = runs.len();
            if n >= 2
                && (runs[n - 1].start == 0
                    || runs[n - 2].len <= runs[n - 1].len
                    || (n >= 3 && runs[n - 3].len <= runs[n - 2].len + runs[n - 1].len)
                    || (n >= 4 && runs[n - 4].len <= runs[n - 3].len + runs[n - 2].len))
            {
                if n >= 3 && runs[n - 3].len < runs[n - 1].len {
                    Some(n - 3)
                } else {
                    Some(n - 2)
                }
            } else {
                None
            }
        }

        #[derive(Clone, Copy)]
        struct Run {
            start: usize,
            len: usize,
        }
    }

    fn insert_head<F>(
        &mut self,
        globals: &mut Globals,
        v: &mut [Value],
        is_less: &mut F,
    ) -> Result<()>
    where
        F: FnMut(&mut Executor, &mut Globals, Value, Value) -> Result<bool>,
    {
        if v.len() >= 2 && is_less(self, globals, v[1], v[0])? {
            unsafe {
                let mut tmp = std::mem::ManuallyDrop::new(std::ptr::read(&v[0]));

                // initially held exactly once.
                let mut hole = InsertionHole {
                    src: &mut *tmp,
                    dest: &mut v[1],
                };
                std::ptr::copy_nonoverlapping(&v[1], &mut v[0], 1);

                for i in 2..v.len() {
                    if !is_less(self, globals, v[i], *tmp)? {
                        break;
                    }
                    std::ptr::copy_nonoverlapping(&v[i], &mut v[i - 1], 1);
                    hole.dest = &mut v[i];
                }
                // `hole` gets dropped and thus copies `tmp` into the remaining hole in `v`.
            }
        }
        return Ok(());

        // When dropped, copies from `src` into `dest`.
        struct InsertionHole<T> {
            src: *mut T,
            dest: *mut T,
        }

        impl<T> Drop for InsertionHole<T> {
            fn drop(&mut self) {
                unsafe {
                    std::ptr::copy_nonoverlapping(self.src, self.dest, 1);
                }
            }
        }
    }

    fn merge<F>(
        &mut self,
        globals: &mut Globals,
        v: &mut [Value],
        mid: usize,
        buf: *mut Value,
        is_less: &mut F,
    ) -> Result<()>
    where
        F: FnMut(&mut Executor, &mut Globals, Value, Value) -> Result<bool>,
    {
        let len = v.len();
        let v = v.as_mut_ptr();
        let (v_mid, v_end) = unsafe { (v.add(mid), v.add(len)) };

        let mut hole;

        if mid <= len - mid {
            // The left run is shorter.
            unsafe {
                std::ptr::copy_nonoverlapping(v, buf, mid);
                hole = MergeHole {
                    start: buf,
                    end: buf.add(mid),
                    dest: v,
                };
            }

            // Initially, these pointers point to the beginnings of their arrays.
            let left = &mut hole.start;
            let mut right = v_mid;
            let out = &mut hole.dest;

            while *left < hole.end && right < v_end {
                // Consume the lesser side.
                // If equal, prefer the left run to maintain stability.
                unsafe {
                    let to_copy = if is_less(self, globals, *right, **left)? {
                        get_and_increment(&mut right)
                    } else {
                        get_and_increment(left)
                    };
                    std::ptr::copy_nonoverlapping(to_copy, get_and_increment(out), 1);
                }
            }
        } else {
            // The right run is shorter.
            unsafe {
                std::ptr::copy_nonoverlapping(v_mid, buf, len - mid);
                hole = MergeHole {
                    start: buf,
                    end: buf.add(len - mid),
                    dest: v_mid,
                };
            }

            // Initially, these pointers point past the ends of their arrays.
            let left = &mut hole.dest;
            let right = &mut hole.end;
            let mut out = v_end;

            while v < *left && buf < *right {
                // Consume the greater side.
                // If equal, prefer the right run to maintain stability.
                unsafe {
                    let to_copy = if is_less(self, globals, *right.offset(-1), *left.offset(-1))? {
                        decrement_and_get(left)
                    } else {
                        decrement_and_get(right)
                    };
                    std::ptr::copy_nonoverlapping(to_copy, decrement_and_get(&mut out), 1);
                }
            }
        };
        return Ok(());
        // Finally, `hole` gets dropped. If the shorter run was not fully consumed, whatever remains of
        // it will now be copied into the hole in `v`.

        unsafe fn get_and_increment<T>(ptr: &mut *mut T) -> *mut T {
            let old = *ptr;
            *ptr = ptr.offset(1);
            old
        }

        unsafe fn decrement_and_get<T>(ptr: &mut *mut T) -> *mut T {
            *ptr = ptr.offset(-1);
            *ptr
        }

        // When dropped, copies the range `start..end` into `dest..`.
        struct MergeHole<T> {
            start: *mut T,
            end: *mut T,
            dest: *mut T,
        }

        impl<T> Drop for MergeHole<T> {
            fn drop(&mut self) {
                // `T` is not a zero-sized type, so it's okay to divide by its size.
                let len = (self.end as usize - self.start as usize) / std::mem::size_of::<T>();
                unsafe {
                    std::ptr::copy_nonoverlapping(self.start, self.dest, len);
                }
            }
        }
    }
}
