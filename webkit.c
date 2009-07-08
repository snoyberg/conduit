/*
 * Copyright (C) 2006, 2007 Apple Inc.
 * Copyright (C) 2007 Alp Toker <alp@atoker.com>
 *
 * Copyright (C) 2009 Michael Snoyman <michael@snoyman.com>
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY APPLE COMPUTER, INC. ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL APPLE COMPUTER, INC. OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include <gtk/gtk.h>
#include <webkit/webkit.h>

static GtkWidget* main_window;
static WebKitWebView* web_view;
static gchar* main_title;

static void
update_title (GtkWindow* window)
{
	GString* string = g_string_new (main_title);
	gchar* title = g_string_free (string, FALSE);
	gtk_window_set_title (window, title);
	g_free (title);
}

static void
title_change_cb (WebKitWebView* web_view, WebKitWebFrame* web_frame, const gchar* title, gpointer data)
{
	if (main_title)
		g_free (main_title);
	main_title = g_strdup (title);
	update_title (GTK_WINDOW (main_window));
}

/*
   static void
   notify_load_status_cb (WebKitWebView* web_view, GParamSpec* pspec, gpointer data)
   {
   if (webkit_web_view_get_load_status (web_view) == WEBKIT_LOAD_COMMITTED) {
   WebKitWebFrame* frame = webkit_web_view_get_main_frame (web_view);
   const gchar* uri = webkit_web_frame_get_uri (frame);
   if (uri)
   gtk_entry_set_text (GTK_ENTRY (uri_entry), uri);
   }
   }
   */

/*static void
  notify_progress_cb (WebKitWebView* web_view, GParamSpec* pspec, gpointer data)
  {
  load_progress = webkit_web_view_get_progress (web_view) * 100;
  update_title (GTK_WINDOW (main_window));
  }
  */

static void
destroy_cb (GtkWidget* widget, gpointer data)
{
	gtk_main_quit ();
}

static GtkWidget*
create_browser ()
{
	GtkWidget* scrolled_window = gtk_scrolled_window_new (NULL, NULL);
	gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

	web_view = WEBKIT_WEB_VIEW (webkit_web_view_new ());
	gtk_container_add (GTK_CONTAINER (scrolled_window), GTK_WIDGET (web_view));

	g_signal_connect (web_view, "title-changed", G_CALLBACK (title_change_cb), web_view);

	return scrolled_window;
}

static GtkWidget*
create_window ()
{
	GtkWidget* window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
	gtk_window_set_default_size (GTK_WINDOW (window), 800, 600);
	gtk_widget_set_name (window, "GtkLauncher");
	g_signal_connect (window, "destroy", G_CALLBACK (destroy_cb), NULL);

	return window;
}

void start_browser(void)
{
	int argc = 0;
	char *argv0 = "";
	char **argv = &argv0;
	gtk_init (&argc, &argv);
	if (!g_thread_supported ())
		g_thread_init (NULL);

	GtkWidget* vbox = gtk_vbox_new (FALSE, 0);
	gtk_box_pack_start (GTK_BOX (vbox), create_browser (), TRUE, TRUE, 0);

	main_window = create_window ();
	gtk_container_add (GTK_CONTAINER (main_window), vbox);

	gchar* uri = (gchar*) ("http://localhost:3000");
	webkit_web_view_open (web_view, uri);

	gtk_widget_grab_focus (GTK_WIDGET (web_view));
	gtk_widget_show_all (main_window);
	gtk_main ();
}
