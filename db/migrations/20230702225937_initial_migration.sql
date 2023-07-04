-- migrate:up
CREATE TABLE synapses (
    id INTEGER PRIMARY KEY,
    content TEXT NOT NULL
);


-- migrate:down

DROP TABLE synapses;
