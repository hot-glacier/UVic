import sys
import struct

class GlobalHeader:

	magic_number = None 
	version_minor = None
	version_major = None
	thiszone = None
	sigfigs = None
	snaplen = None
	network = None

    # Initialize the GlobalHeader with all relevant attributes
	def __init__(self, buffer):
		self.magic_number, self.version_minor, self.version_major, self.thiszone, self.sigfigs, self.snaplen, self.network = struct.unpack('IHHiIII', buffer)
		
class PacketHeader:

	ts_sec = None
	ts_usec = None
	incl_len = None
	orig_len = None

    # Initialize PacketHeader with empty attributes (as ints)
	def __init__(self):
		self.ts_sec = 0
		self.ts_usec = 0
		self.incl_len = 0
		self.orig_len = 0

    # Set the header attributes
	def set_header(self, buffer):
		self.ts_sec, self.ts_usec, self.incl_len, self.orig_len = struct.unpack('IIII', buffer)
		
class IPHeader:

	ihl = None
	total_length = None
	identification = None
	flags = None
	fragment_offset = None
	ttl = None
	protocol = None
	src_ip = None
	dst_ip = None

    # Set the IHL
	def set_ihl(self, value):
		IP_Version = struct.unpack('B', value)[0]
		self.ihl = (IP_Version & 0xf) * 4

    # Set the total length using a buffer containing a 16-bit integer
	def set_total_len(self, buffer):
		self.total_length = (buffer[0]<<8)|buffer[1]
	
    # Set the IPs of the source and the destination
	def set_ip(self, buffer1, buffer2):
		src = struct.unpack('BBBB', buffer1)
		dst = struct.unpack('BBBB', buffer2)
		self.src_ip = str(src[0]) + '.' + str(src[1]) + '.' + str(src[2]) + '.' + str(src[3])
		self.dst_ip = str(dst[0]) + '.' + str(dst[1]) + '.' + str(dst[2]) + '.' + str(dst[3])

    # Set the identification using a buffer containing a 16-bit integer
	def set_identification(self, buffer):
		self.identification = struct.unpack('!H', buffer)[0]

    # Set fragment offset using a buffer containing a 3-bit integer (flags) and a 13-bit integer (offset)
	def set_fragment_offset(self, buffer):
		self.flags = hex((buffer[0] & 0xE0) >> 5)
		self.fragment_offset = ((buffer[0] & 0x1F) << 8 | buffer[1]) * 8

    # Set the TTL
	def set_ttl(self, buffer):
		self.ttl = struct.unpack('B', buffer)[0]

    # Set the transport protocol
	def set_protocol(self, buffer):
		self.protocol = struct.unpack('B', buffer)[0]
		
class UDPHeader:
	
	src_port = None
	dst_port = None
	udp_len = None
	checksum = None

    # The following 4 functions set the attributes of the UDPHeader using a buffer with 16-bit integers each

	def set_src_port(self, buffer):
		self.src_port = struct.unpack('!H', buffer)[0]

	def set_dst_port(self, buffer):
		self.dst_port = struct.unpack('!H', buffer)[0]

	def set_udp_len(self, buffer):
		self.udp_len = struct.unpack('!H', buffer)[0]

	def set_checksum(self, buffer):
		self.checksum = struct.unpack('!H', buffer)[0]
		
class ICMPHeader:

	type_num = None
	code = None
	src_port = None
	dst_port = None
	sequence = None

    # The following 5 functions set the attributes of the ICMPHeader using a buffer with 8-bit and 16-bit integers

	def set_type(self, buffer):
		self.type_num = int(struct.unpack('B', buffer)[0])

	def set_code(self, buffer):
		self.code = int(struct.unpack('B', buffer)[0])

	def set_src_port(self, buffer):
		self.src_port = struct.unpack('!H', buffer)[0]

	def set_dst_port(self, buffer):
		self.dst_port = struct.unpack('!H', buffer)[0]

	def set_sequence(self, buffer):
		self.sequence = struct.unpack('!H', buffer)[0]

class Packet:

	header = None		# PacketHeader
	ip = None			# IPHeader
	udp = None			# UDPHeader
	icmp = None			# ICMPHeader
	data = None
	payload = None
	timestamp = None

    # Initialize attibutes
	def __init__(self):
		self.header = PacketHeader()
		self.ip = IPHeader()
		self.udp = UDPHeader()
		self.icmp = ICMPHeader()
		self.data = b'' #byte type
		self.payload = 0
		self.timestamp = 0

    # Set the PacketHeader 
	def set_header(self, buffer):
		self.header.set_header(buffer)
	
    # Set the IPHeader
	def set_ip(self):
		offset = 14 # Skip first 14 bytes (Ethernet Header)
		
        # Pass the data in to set the IPHeader fields
		self.ip.set_ihl(self.data[offset+0: offset+1])
		self.ip.set_total_len(self.data[offset+2: offset+4])
		self.ip.set_identification(self.data[offset+4: offset+6])
		self.ip.set_fragment_offset(self.data[offset+6: offset+8])
		self.ip.set_ttl(self.data[offset+8: offset+9])
		self.ip.set_protocol(self.data[offset+9: offset+10])
		self.ip.set_ip(self.data[offset+12: offset+16], self.data[offset+16: offset+20])

    # Set the UDPHeader
	def set_udp(self):
		offset = 14 + self.ip.ihl # Skip first 14 bytes (Ethernet Header) plus the IHL (dynamic Internet Header Length)
		
        # Pass the data in to set the UDPHeader fields
		self.udp.set_src_port(self.data[offset+0: offset+2])
		self.udp.set_dst_port(self.data[offset+2: offset+4])
		self.udp.set_udp_len(self.data[offset+4: offset+6])
		self.udp.set_checksum(self.data[offset+6: offset+8])

    # Set the ICMPHeader
	def set_icmp(self):
		offset = 14 + self.ip.ihl # Same idea as in UDP, skip the Ethernet and IP headers
		
        # Setting the ICMP type and code
		self.icmp.set_type(self.data[offset+0: offset+1])
		self.icmp.set_code(self.data[offset+1: offset+2])
		
        # Check if the message is an Echo Request/Reply
		is_echo_message = (self.icmp.type_num == 8 or self.icmp.type_num == 0)
		
        # If so, set the sequence number as the last 2 bytes of the 8-byte header
		if is_echo_message:
			self.icmp.set_sequence(self.data[offset + 6: offset + 8])
			
        # Otherwise, it is an ICMP error message
		else:
			# IP header starts after 8-byte ICMP header
			original_ip_start = offset + 8
			
            # TCP/UDP header starts after 20-byte IP header
			transport_header_start = original_ip_start + 20
			
            # Get the src/dst poprts of the original faulty packet
			if transport_header_start + 4 <= len(self.data):
				self.icmp.set_src_port(self.data[transport_header_start + 0: transport_header_start + 2])
				self.icmp.set_dst_port(self.data[transport_header_start + 2: transport_header_start + 4])
			else:
				self.icmp.src_port = 0
				self.icmp.dst_port = 0

    # Store the packet data
	def set_data(self, buffer):
		self.data = buffer

    # Set the packet numbers in sequence
	def set_number(self, value):
		self.number = value

    # Convert raw timestamp into relative timestamp
	def set_timestamp(self, orig_time):
		self.timestamp = round((self.header.ts_sec + self.header.ts_usec * 0.000000001 - orig_time) * 1000, 6)
		
    # Function for calculating RTT value between reply (self) and request (p)
	def set_rtt(self, p):
		rtt = p.timestamp - self.timestamp
		self.RTT_value = round(rtt, 8)

#############################################################
##################### Program beginning #####################
#############################################################

# Get filename from command line
if len(sys.argv) != 2:
	print('Incorrect usage, correct syntax: python3 TraceAnalyzer.py <PCAP_file_name.cap>')
	exit()

# Open the PCAP file in binary mode
f = open(sys.argv[1], 'rb')

# First 24 bytes are the GlobalHeader
global_header = GlobalHeader(f.read(24))

# Protocols we care about in this program
protocol_map = {1: 'ICMP', 17: 'UDP'}
protocol_used = {}

# Packet lists and counter
src = []
dst = []
start_time = None
packets = 0

# Parse the packets in the PCAP file
while True:
	packets += 1

	# First 16 bytes are the packet header
	data = f.read(16)

	# If there is an empty byte, we have reached the end and exit
	if data == b'':
		break

	# Initialize the packet and set the header and its number (0, 1, 2,..., n)
	packet = Packet()
	packet.set_header(data)
	packet.set_number(packets)

	# Set the timing relative to the starting time of the PCAP file
	if start_time is None:
		start_time = round(packet.header.ts_sec + packet.header.ts_usec * 0.000001, 6)
	
	# The next <incl_len> bytes contain the packet data
	packet.set_data(f.read(packet.header.incl_len))

	# Parse IPHeader
	packet.set_ip()

	# Depending on protocol, parse either the ICMPHeader or UDPHeader
	if packet.ip.protocol == 1:
		packet.set_icmp()
		dst.append(packet)
		protocol_used[1] = 'ICMP'
	
	if packet.ip.protocol == 17:
		packet.set_udp()
		src.append(packet)
		# Make sure our UDP packet is relevant
		if not 33434 <= packet.udp.dst_port <= 33529:
			continue
		protocol_used[17] = 'UDP'
		
	# Skip packets with protocols we don't care about
	if packet.ip.protocol not in protocol_map:
		continue

# Windows - ICMP packets
if any(p.icmp.type_num == 8 for p in dst):

	icmp_all = dst
	src = []
	dst = []
	
	# Iterate through and separate packets by src (Echo Request) and dst (Echo Reply or Time Exceeded)
	for p in icmp_all:
		if p.icmp.type_num == 8:
			src.append(p)
		if p.icmp.type_num == 11 or p.icmp.type_num == 0:
			dst.append(p)

	# Lists to contain intermediate hops and a dictionary to contain calculated RTTs
	intermediate = []
	intermediate_pkts = []
	rtts = {}

	# Identify matching packets
	for i in src:
		for k in dst:
			if i.icmp.sequence == k.icmp.sequence:
				if k.ip.src_ip not in intermediate:
					intermediate.append(k.ip.src_ip)
					intermediate_pkts.append(k)
					rtts[k.ip.src_ip] = []
				
				# Calculating RTT
				i.set_timestamp(start_time)
				k.set_timestamp(start_time)
				rtts[k.ip.src_ip].append(k.timestamp-i.timestamp)

# Linux - UDP packets
else:
	# Lists to contain intermediate hops and a dictionary to contain calculated RTTs
	intermediate = []
	intermediate_pkts = []
	rtts = {}

	# Identify matching packets
	for i in src:
		for k in dst:
			if i.udp.src_port == k.icmp.src_port:
				if k.ip.src_ip not in intermediate:
					intermediate.append(k.ip.src_ip)
					intermediate_pkts.append(k)
					rtts[k.ip.src_ip] = []
				
				# Calculating RTT
				i.set_timestamp(start_time)
				k.set_timestamp(start_time)
				rtts[k.ip.src_ip].append(k.timestamp-i.timestamp)

identities = {}

# Fragmented datagram resolution
for packet in src:
	if packet.ip.identification not in identities:
		identities[packet.ip.identification] = []
	identities[packet.ip.identification].append(packet)

# Count fragments - a key with 1 packet is not fragmented, if there are more than 1 than it is fragmented
frag_count = 0
for identity in identities:
	if len(identities[identity]) > 1:
		frag_count += 1

#############################################################
######################## R1 Output ##########################
#############################################################

# Print out the results of the program according to R1 specifications

# IP Addresses and IP protocols
print('The IP address of the source node:', src[0].ip.src_ip)
print('The IP address of ultimate destination node:', src[0].ip.dst_ip)
print('The IP addresses of the intermediate destination nodes:')
for i in range(len(intermediate)-1):
	print(f'\tRouter {i+1}: {intermediate[i]}')

print('\nThe values in the protocol field of IP headers:')
for protocol in sorted(protocol_used):
	print(f'\t{protocol}: {protocol_used[protocol]}')

print()

# Fragments, if no fragments print once, otherwise iterate through and print all fragments
if frag_count == 0:
	print('The number of fragments created from the original datagram is:', frag_count)
	print('The offset of the last fragment is:', frag_count, '\n')
else:
	for identity in identities:
		if len(identities[identity]) > 1:
			print('The number of fragments created from the original datagram', identity, 'is:', len(identities[identity]))
			
			offset = max(packet.ip.fragment_offset for packet in identities[identity])
			print('The offset of the last fragment is:', offset, '\n')

# RTT, average time, and standard deviations
for i in range(len(intermediate)):
	avg_RTT = round(sum(rtts[intermediate[i]]) / len(rtts[intermediate[i]]), 6)
	std_dev = round( (sum(pow(x-avg_RTT,2) for x in rtts[intermediate[i]]) / len(rtts[intermediate[i]]))**(1/2), 6)
	print('The avg RTT between', src[0].ip.src_ip, 'and', intermediate[i], 'is:', avg_RTT, 'ms, the s.d. is:', std_dev, 'ms')