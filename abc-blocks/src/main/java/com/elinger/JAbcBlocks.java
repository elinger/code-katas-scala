package com.elinger;


import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class JAbcBlocks {

    public static void main(String[] args) throws IOException{
        List<Block> blocks = new ArrayList<>();
        InputStream resourceAsStream = JAbcBlocks.class.getClassLoader().getResourceAsStream("blocks.txt");
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(resourceAsStream))) {
            while (reader.ready()) {
                String line = reader.readLine();
                blocks.add(Block.of(line.charAt(1), line.charAt(3)));
            }
        }

        System.out.println(canMakeWord("A", blocks));
        System.out.println(canMakeWord("BARK", blocks));
        System.out.println(canMakeWord("TREAT", blocks));
        System.out.println(canMakeWord("SQUAD", blocks));
        System.out.println(canMakeWord("CONFUSE", blocks));
        System.out.println(canMakeWord("BOOK", blocks));
        System.out.println(canMakeWord("COMMON", blocks));
    }

    static class Block {
        char letter1;
        char letter2;

        public static Block of(char letter1, char letter2) {
            Block block = new Block();
            block.letter1 = letter1;
            block.letter2 = letter2;
            return block;
        }

        boolean hasLetter(char letter) {
            return letter1 == letter || letter2 == letter;
        }
    }

    public static boolean canMakeWord(String word, List<Block> blocks) {
        char[] wordLetters = word.toCharArray();

        HashMap<Character, Set<Integer>> letterToBlockIndices = new HashMap<>();

        for (int i = 0; i < blocks.size(); i++) {
            Block block = blocks.get(i);
            for (char letter : wordLetters) {
                if (block.hasLetter(letter)) {
                    Set<Integer> indices = letterToBlockIndices.computeIfAbsent(letter, k -> new HashSet<>());
                    indices.add(i);
                }
            }
        }

        long distinctLetterCount = word.chars().distinct().count();

        if (distinctLetterCount > letterToBlockIndices.keySet().size()) {
            // less num of useful blocks than the number of letters
            return false;
        }

        return utilCanMakeWord(0, new HashSet<>(), wordLetters, letterToBlockIndices);
    }

    private static boolean utilCanMakeWord(
            int currentLetterIndex,
            Set<Integer> usedBlocksIndices,
            char[] word,
            HashMap<Character, Set<Integer>> letterToBlockIndices) {

        if (word.length == currentLetterIndex) {
            // if we come so far, there is a block for each letter
            return true;
        }

        char letter = word[currentLetterIndex];
        boolean result = false;
        for (int blockIndex : letterToBlockIndices.get(letter)) {
           if (!usedBlocksIndices.contains(blockIndex)) {
              Set<Integer> newUsedBlocksIndices = new HashSet<>(usedBlocksIndices);
              newUsedBlocksIndices.add(blockIndex);
              result = utilCanMakeWord(currentLetterIndex + 1, newUsedBlocksIndices, word, letterToBlockIndices);
              // if one combination is found, stop searching
              if (result) break;
           }
        }

        return result;
    }
}
