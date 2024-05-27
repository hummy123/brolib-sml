bench: gap_buffer_svelte gap_buffer_rust gap_buffer_seph gap_buffer_automerge
	./gap_buffer_svelte
	./gap_buffer_rust
	./gap_buffer_seph
	./gap_buffer_automerge

gap_buffer_svelte: 
	mlton gap_buffer_svelte.mlb

gap_buffer_rust:
	mlton gap_buffer_rust.mlb

gap_buffer_seph:
	mlton gap_buffer_seph.mlb

gap_buffer_automerge:
	mlton gap_buffer_automerge.mlb

clean:
	rm -f gap_buffer_svelte gap_buffer_rust gap_buffer_svelte gap_buffer_automerge
