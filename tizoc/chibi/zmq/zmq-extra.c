/* -*- c-file-style: "stroustrup" -*- */

#include <chibi/eval.h>
#include <chibi/sexp.h>
#include <stdint.h>
#include <zmq.h>

static sexp sexp_make_c_string (sexp ctx, sexp self,
                                sexp bytes_cpointer, sexp count) {
    if (! (sexp_pointerp(bytes_cpointer)
           && (sexp_pointer_tag(bytes_cpointer) == SEXP_CPOINTER))) {
        return sexp_type_exception(ctx, self, SEXP_CPOINTER, bytes_cpointer);
    }

    if (! sexp_exact_integerp(count)) {
        return sexp_type_exception(ctx, self, SEXP_FIXNUM, count);
    }

    return sexp_c_string(ctx, (void*)sexp_cpointer_value(bytes_cpointer),
                         sexp_sint_value(count));
}

static sexp sexp_string_bytes_count (sexp ctx, sexp self,
                                     sexp string) {
    if (! sexp_stringp(string)) {
        return sexp_type_exception(ctx, self, SEXP_STRING, string);
    }

    return sexp_make_integer(ctx, sexp_string_length(string));
}

static sexp sexp_zmq_msg_set_string_data (sexp ctx, sexp self,
                                          sexp message, sexp string) {
    zmq_msg_t *msg = sexp_cpointer_value(message);

    memcpy(zmq_msg_data(msg),
           sexp_string_data(string),
           sexp_string_length(string));

    return SEXP_VOID;
}

static sexp sexp_zmq_select (sexp ctx, sexp self,
                             sexp in, sexp out, sexp err, sexp timeout) {
    sexp_gc_var4(res, in_res, out_res, err_res);
    sexp_gc_preserve4(ctx, res, in_res, out_res, err_res);
    int i, total, poll_result;
    int count =
        sexp_vector_length(in) +
        sexp_vector_length(out) +
        sexp_vector_length(err);
    zmq_pollitem_t poll_items[count];

    total = 0;

    for (i = 0; i < sexp_vector_length(in); i++, total++) {
        poll_items[total].socket =
            (void*)sexp_cpointer_value(sexp_vector_data(in)[i]);
        poll_items[total].events = ZMQ_POLLIN;
    }

    for (i = 0; i < sexp_vector_length(out); i++, total++) {
        poll_items[total].socket =
            (void*)sexp_cpointer_value(sexp_vector_data(out)[i]);
        poll_items[total].events = ZMQ_POLLOUT;
    }

    for (i = 0; i < sexp_vector_length(err); i++, total++) {
        poll_items[total].socket =
            (void*)sexp_cpointer_value(sexp_vector_data(err)[i]);
        poll_items[total].events = ZMQ_POLLERR;
    }

    poll_result = zmq_poll(poll_items, count, sexp_sint_value(timeout));

    if (poll_result < 0) {
        /*
          EFAULT: At least one of the members of the items array refers to a socket belonging to a different application thread.
          ETERM: At least one of the members of the items array refers to a socket whose associated ØMQ context was terminated.
          EFAULT: The provided items was not valid (NULL).
        */
        res = SEXP_FALSE; /* FIXME: raise exception? */
    } else if (poll_result == 0) {
        res = SEXP_NULL;
    } else {
        in_res = SEXP_NULL;
        out_res = SEXP_NULL;
        err_res = SEXP_NULL;

        total = 0;

        for (i = 0; i < sexp_vector_length(in); i++, total++) {
            if (poll_items[total].revents & ZMQ_POLLIN) {
                in_res = sexp_cons(ctx, sexp_vector_ref(in, i), in_res);
            }
        }

        for (i = 0; i < sexp_vector_length(out); i++, total++) {
            if (poll_items[total].revents & ZMQ_POLLOUT) {
                out_res = sexp_cons(ctx, sexp_vector_ref(out, i), out_res);
            }
        }

        for (i = 0; i < sexp_vector_length(err); i++, total++) {
            if (poll_items[total].revents & ZMQ_POLLERR) {
                err_res = sexp_cons(ctx, sexp_vector_ref(err, i), err_res);
            }
        }

        res = SEXP_NULL;
        res = sexp_cons(ctx, err_res, res);
        res = sexp_cons(ctx, out_res, res);
        res = sexp_cons(ctx, in_res, res);
    }

    sexp_gc_release4(ctx);

    return res;
}

static sexp sexp_zmq_getsockopt_string (sexp ctx, sexp self,
                                        sexp arg0, sexp arg1) {
    int err;
    char value[255];
    size_t size = sizeof(value);

    sexp_gc_var1(res);
    sexp_gc_preserve1(ctx, res);

    err = zmq_getsockopt(sexp_cpointer_value(arg0),
                         sexp_sint_value(arg1), value, &size);

    if (err) {
        // TODO: check error?
        res = SEXP_FALSE;
    } else {
        res = sexp_c_string(ctx, value, size);
    }

    sexp_gc_release1(ctx);

    return res;
}

static sexp sexp_zmq_getsockopt_uint64 (sexp ctx, sexp self,
                                        sexp arg0, sexp arg1) {
    int err;
    uint64_t value;
    size_t size = sizeof(value);

    err = zmq_getsockopt(sexp_cpointer_value(arg0),
                         sexp_sint_value(arg1), &value, &size);

    if (err) {
        // TODO: check error?
        return SEXP_FALSE;
    } else {
        return sexp_make_integer(ctx, value);
    }
}

static sexp sexp_zmq_getsockopt_int64 (sexp ctx, sexp self,
                                       sexp arg0, sexp arg1) {
    int err;
    int64_t value;
    size_t size = sizeof(value);

    err = zmq_getsockopt(sexp_cpointer_value(arg0),
                         sexp_sint_value(arg1), &value, &size);

    if (err) {
        // TODO: check error?
        return SEXP_FALSE;
    } else {
        return sexp_make_integer(ctx, value);
    }
}
