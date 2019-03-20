import ranger.gui.context
import ranger.gui.widgets.browsercolumn

ranger.gui.context.CONTEXT_KEYS.append('video_linked')
ranger.gui.context.Context.video_linked = False
ranger.gui.context.CONTEXT_KEYS.append('video_linked_tvshow')
ranger.gui.context.Context.video_linked_tvshow = False
ranger.gui.context.CONTEXT_KEYS.append('video_linked_badname')
ranger.gui.context.Context.video_linked_badname = False


OLD_HOOK_BEFORE_DRAWING = ranger.gui.widgets.browsercolumn.hook_before_drawing


def new_hook_before_drawing(fsobject, color_list):
    if fsobject.dirname == '/home/justf/video-linked':
        color_list.append('video_linked')
        if fsobject.basename.startswith('[T]'):
            color_list.append('video_linked_tvshow')
        elif not fsobject.basename.startswith('[M]'):
            color_list.append('video_linked_badname')

    return OLD_HOOK_BEFORE_DRAWING(fsobject, color_list)


ranger.gui.widgets.browsercolumn.hook_before_drawing = new_hook_before_drawing
