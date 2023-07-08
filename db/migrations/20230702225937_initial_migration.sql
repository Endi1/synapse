-- migrate:up
CREATE TABLE synapses (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    content TEXT NOT NULL
);


-- migrate:down

DROP TABLE synapses;
