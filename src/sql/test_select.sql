.mode line
SELECT info.title, content.text FROM
  info LEFT OUTER JOIN content
    ON info.id = content.id;
