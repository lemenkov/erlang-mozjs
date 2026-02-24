// SPDX-FileCopyrightText: 2009-2010 Basho Technologies
// SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
// SPDX-License-Identifier: Apache-2.0

#ifndef SPIDERMONKEY_INTERFACE_H
#define SPIDERMONKEY_INTERFACE_H

#include <jsapi.h>
#include <string>

class spidermonkey_state
{
  public:
    int branch_count = 0;
    bool terminate = false;
    bool error = false;
    spidermonkey_state() {}
    ~spidermonkey_state() { free_error(); }
    void replace_error(const char* m = "undefined error", unsigned int l = 0,
                       const char* os = "<unknown>");
    char* error_to_json();

  private:
    unsigned int lineno = 0;
    std::string* msg = nullptr;
    std::string* offending_source = nullptr;
    void free_error()
    {
        if (error)
        {
            error = false;
            delete msg;
            delete offending_source;
        }
    }
};

class spidermonkey_vm
{
  public:
    JSContext* context;
    JS::PersistentRootedObject global;

    spidermonkey_vm(size_t thread_stack, uint32_t heap_size);
    ~spidermonkey_vm();

    // Erlang binaries aren't null-terminated, so we have to provide length explicitly
    bool sm_eval(const char* filename, size_t filename_length, const char* code,
                 size_t code_length, char** output, int handle_retval);
    void sm_stop();

  private:
    void check_js_exception();
};

#endif // SPIDERMONKEY_INTERFACE_H
