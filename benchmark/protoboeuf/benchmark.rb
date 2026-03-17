# Protoboeuf decoder
require_relative 'benchmark_pb'

Dir.chdir __dir__
FAKE_MSG_BINS = Marshal.load(File.binread('encoded_msgs.bin'))

20.times do
  FAKE_MSG_BINS.each { |bin| p ProtoBoeuf::ParkingLot.decode bin }
end
