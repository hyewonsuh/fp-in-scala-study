package stream;


import java.util.stream.IntStream;

import static java.util.stream.Collectors.toList;

/**
 * Created by Wendy on 2016. 2. 12..
 */
public class StreamExamples {

    public static void main(String[] args) {
        System.out.println(IntStream.range(1, 10)
                                    .map(i -> i + 10)
                                    .filter(i -> i % 2 == 0).sum());

        System.out.println(IntStream.iterate(1, i -> i + 1)
                                    .map(i -> i + 10)
                                    .filter(i -> i % 2 == 0)
                                    .sum());
    }
}
