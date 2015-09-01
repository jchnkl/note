.mode line
SELECT note_meta_name, note_content FROM notes_meta
  LEFT OUTER JOIN notes_note ON notes_meta.note_meta_id = notes_note.note_id;
