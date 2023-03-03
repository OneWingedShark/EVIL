With
EVIL.Util.Files,
Testbed.Pure_IO,
Testbed.Instantiations.Strings;

Use
Testbed.Pure_IO;

Package Testbed.Instantiations.Safe_IO is New EVIL.Util.Files
      (
	S		=> Testbed.Instantiations.Strings,
	File_Type	=> Testbed.Pure_IO.File_Type,
	File_Mode	=> Testbed.Pure_IO.File_Mode,
	Stream_Access	=> Testbed.Pure_IO.Stream_Access
      );
