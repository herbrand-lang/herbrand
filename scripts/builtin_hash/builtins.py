from sys import argv

def hashmap_hash(size, key):
	i = 0
	crc = 0xFFFFFFFF
	while i < len(key):
		byte = ord(key[i])
		crc = crc^byte
		for j in range(7,-1,-1):
			mask = -(crc & 1)
			crc = (crc >> 1)^(0xEDB88320 & mask)
		i += 1
	return ~crc % size

def hashmap_gen(path):
	keys = []
	handlers = []
	arities = []
	f = open(path, 'r')
	i = 0
	for line in f:
		line = line.strip().split(' ')
		keys.append(line[0])
		handlers.append(line[1])
		arities.append(int(line[2]))
		i += 1
	f.close()
	size = len(keys)
	n = size
	while True:
		dist = map(lambda x: hashmap_hash(n, x), keys)
		if len(set(dist)) == size:
			break
		else:
			n += 1
	dic = dict()
	for i in range(size):
		dic[dist[i]] = (keys[i], handlers[i], arities[i])
	# Print array of keys
	string = "char *builtin_keys[BUILTIN_HASH_SIZE] = {\n\t"
	line = ""
	for i in range(n):
		if len(line) > 76:
			string += line + "\n\t"
			line = ""
		char = "};\n" if i == n-1 else ", "
		if dic.get(i) != None:
			line += "\"%s\"%s" % (dic[i][0], char)
		else:
			line += "NULL%s" % char
	string += line
	print string
	# Print array of handlers
	string = "void (*builtin_handlers[BUILTIN_HASH_SIZE])() = {\n\t"
	line = ""
	for i in range(n):
		if len(line) > 76:
			string += line + "\n\t"
			line = ""
		char = "};\n" if i == n-1 else ", "
		if dic.get(i) != None:
			line += "builtin_%s%s" % (dic[i][1], char)
		else:
			line += "NULL%s" % char
	string += line
	print string
	# Print array of arities
	string = "int builtin_arities[BUILTIN_HASH_SIZE] = {\n\t"
	line = ""
	for i in range(n):
		if len(line) > 76:
			string += line + "\n\t"
			line = ""
		char = "};\n" if i == n-1 else ", "
		if dic.get(i) != None:
			line += "%d%s" % (dic[i][2], char)
		else:
			line += "0%s" % char
	string += line
	print string
	print n

hashmap_gen(argv[1])