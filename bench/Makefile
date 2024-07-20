bench: gap_buffer_svelte gap_buffer_rust gap_buffer_seph gap_buffer_automerge line_gap_svelte line_gap_rust line_gap_seph line_gap_automerge rope_svelte rope_rust rope_seph rope_automerge
	hyperfine './gap_buffer_svelte' './rope_svelte' './line_gap_svelte' --export-markdown svelte.md
	hyperfine './gap_buffer_rust' './rope_rust'  './line_gap_rust' --export-markdown rust.md
	hyperfine './gap_buffer_seph' './rope_seph' './line_gap_seph' --export-markdown seph.md
	hyperfine './gap_buffer_automerge' './rope_automerge' './line_gap_automerge' --export-markdown automerge.md

gap_buffer_svelte: 
	mlton gap_buffer_svelte.mlb
	
gap_buffer_rust:
	mlton gap_buffer_rust.mlb

gap_buffer_seph:
	mlton gap_buffer_seph.mlb

gap_buffer_automerge:
	mlton gap_buffer_automerge.mlb

line_gap_svelte: 
	mlton line_gap_svelte.mlb
	
line_gap_rust:
	mlton line_gap_rust.mlb

line_gap_seph:
	mlton line_gap_seph.mlb

line_gap_automerge:
	mlton line_gap_automerge.mlb

rope_svelte:
	mlton rope_svelte.mlb

rope_rust:
	mlton rope_rust.mlb

rope_seph:
	mlton rope_seph.mlb

rope_automerge:
	mlton rope_automerge.mlb

clean:
	rm -f gap_buffer_svelte gap_buffer_rust gap_buffer_seph gap_buffer_automerge 
	rm -f line_gap_svelte line_gap_rust line_gap_seph line_gap_automerge
	rm -f rope_svelte rope_rust rope_seph rope_automerge
	rm -f svelte.md rust.md seph.md automerge.md
