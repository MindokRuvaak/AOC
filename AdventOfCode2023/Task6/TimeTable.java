import java.util.ArrayList;
import java.util.List;

public class TimeTable {
    private final List<Race> races;
    private final Race race;

    void TimeTableOLD(List<String[]> stringImport) {
        // this.races = stringArrayToRace(stringImport);
    }

    TimeTable(List<String> stringImport) {
        this.races = null;
        race = stringToRace(stringImport);
    }

    private Race stringToRace(List<String> stringImport) {
        return new Race(Long.valueOf(stringImport.get(0)), Long.valueOf(stringImport.get(1)));
    }

    private static List<Race> stringArrayToRaceOLD(List<String[]> stringImport) {
        List<Race> importedRaces = new ArrayList<>();
        for (String raceStrings[] : stringImport) {
            long imported[] = readImport(raceStrings);
            importedRaces.add(new Race(imported[0], imported[1]));
        }
        return importedRaces;
    }

    private static long[] readImport(String[] raceStrings) {
        long[] timeDistance = new long[2];
        for (int i = 0; i < timeDistance.length; i++) {
            timeDistance[i] = Integer.valueOf(raceStrings[i]);
        }
        return timeDistance;
    }

    public long numOfWinningChargeTimes() {
        return race.winningChargeTimes().size();
    }

    public List<Long> numOfWinningChargeTimesOLD() {
        List<Long> numWinningChargeTimes = new ArrayList<>();
        for (Race race : races) {
            numWinningChargeTimes.add((long)race.winningChargeTimes().size());
        }
        return numWinningChargeTimes;
    }

    public List<Race> getRaces() {
        return races;
    }

    private static class Race {
        private final long timeAllowed;
        private final long distanceToBeat;

        public Race(long timeAllowed, long distanceToBeat) {
            this.timeAllowed = timeAllowed;
            this.distanceToBeat = distanceToBeat;
        }

        public List<Long> winningChargeTimes() {
            List<Long> chargeTimes = new ArrayList<>();
            for (long chargeTime = 0; chargeTime <= timeAllowed; chargeTime++) {
                // int velocity = chargeTime;
                // int remainingTime = timeAllowed - chargeTime;
                // int distanceTravelled = chargeTime*(timeAllowed - chargeTime);
                if (chargeTime * (timeAllowed - chargeTime) > distanceToBeat) {
                    chargeTimes.add(chargeTime);
                }
            }
            return chargeTimes;
        }

        public long getTimeAllowed() {
            return timeAllowed;
        }

        public long getDistanceToBeat() {
            return distanceToBeat;
        }
    }
}
